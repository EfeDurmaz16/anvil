package depgraph

import (
	"encoding/json"
	"strings"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/ir"
)

// Helper types for building test SemanticGraphs
type testModule struct {
	name      string
	lang      string
	functions []testFunc
	dataTypes []testDataType
	ioContracts []testIOContract
}

type testFunc struct {
	name  string
	calls []string
}

type testDataType struct {
	name string
}

type testIOContract struct {
	name      string
	kind      ir.IOKind
	direction ir.IODirection
}

func makeTestGraph(modules ...testModule) *ir.SemanticGraph {
	sg := &ir.SemanticGraph{}
	for _, m := range modules {
		mod := &ir.Module{Name: m.name, Language: m.lang}
		for _, f := range m.functions {
			mod.Functions = append(mod.Functions, &ir.Function{
				Name:  f.name,
				Calls: f.calls,
			})
		}
		for _, dt := range m.dataTypes {
			mod.DataTypes = append(mod.DataTypes, &ir.DataType{
				Name: dt.name,
				Kind: ir.TypeStruct,
			})
		}
		for _, io := range m.ioContracts {
			mod.IOContracts = append(mod.IOContracts, &ir.IOContract{
				Name:      io.name,
				Kind:      io.kind,
				Direction: io.direction,
			})
		}
		sg.Modules = append(sg.Modules, mod)
	}
	return sg
}

// Analyzer Tests

func TestAnalyze_EmptyGraph(t *testing.T) {
	sg := &ir.SemanticGraph{}
	g := Analyze(sg)

	if len(g.Nodes) != 0 {
		t.Errorf("expected 0 nodes, got %d", len(g.Nodes))
	}
	if len(g.Edges) != 0 {
		t.Errorf("expected 0 edges, got %d", len(g.Edges))
	}
	if g.Stats.TotalNodes != 0 {
		t.Errorf("expected stats.TotalNodes=0, got %d", g.Stats.TotalNodes)
	}
	if g.Stats.ConnectedComponents != 0 {
		t.Errorf("expected 0 components, got %d", g.Stats.ConnectedComponents)
	}
}

func TestAnalyze_SingleModule(t *testing.T) {
	sg := makeTestGraph(testModule{
		name: "module1",
		lang: "COBOL",
		functions: []testFunc{
			{name: "func1", calls: []string{"func2"}},
			{name: "func2", calls: nil},
		},
	})

	g := Analyze(sg)

	// Expected: 1 module node + 2 function nodes = 3
	if g.Stats.ModuleCount != 1 {
		t.Errorf("expected 1 module, got %d", g.Stats.ModuleCount)
	}
	if g.Stats.FunctionCount != 2 {
		t.Errorf("expected 2 functions, got %d", g.Stats.FunctionCount)
	}
	if g.Stats.TotalNodes != 3 {
		t.Errorf("expected 3 total nodes, got %d", g.Stats.TotalNodes)
	}

	// Check for module->function contains edges
	containsEdges := countEdgesByKind(g, EdgeContains)
	if containsEdges != 2 {
		t.Errorf("expected 2 contains edges, got %d", containsEdges)
	}

	// Check for call edge func1->func2
	callEdges := countEdgesByKind(g, EdgeCalls)
	if callEdges != 1 {
		t.Errorf("expected 1 call edge, got %d", callEdges)
	}
}

func TestAnalyze_MultiModule(t *testing.T) {
	sg := makeTestGraph(
		testModule{
			name: "module1",
			lang: "COBOL",
			functions: []testFunc{
				{name: "func1", calls: []string{"func2"}},
			},
		},
		testModule{
			name: "module2",
			lang: "COBOL",
			functions: []testFunc{
				{name: "func2", calls: nil},
			},
		},
	)

	g := Analyze(sg)

	if g.Stats.ModuleCount != 2 {
		t.Errorf("expected 2 modules, got %d", g.Stats.ModuleCount)
	}
	if g.Stats.FunctionCount != 2 {
		t.Errorf("expected 2 functions, got %d", g.Stats.FunctionCount)
	}

	// Check for module-level dependency edge
	moduleDeps := countEdgesByKind(g, EdgeDependsOn)
	if moduleDeps != 1 {
		t.Errorf("expected 1 module dependency edge, got %d", moduleDeps)
	}

	// Verify the dependency is from module1 to module2
	found := false
	for _, e := range g.Edges {
		if e.Kind == EdgeDependsOn && e.From == "mod:module1" && e.To == "mod:module2" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected module1->module2 dependency edge not found")
	}
}

func TestAnalyze_DataTypes(t *testing.T) {
	sg := makeTestGraph(testModule{
		name: "module1",
		lang: "COBOL",
		dataTypes: []testDataType{
			{name: "Customer"},
			{name: "Order"},
		},
	})

	g := Analyze(sg)

	if g.Stats.DataTypeCount != 2 {
		t.Errorf("expected 2 data types, got %d", g.Stats.DataTypeCount)
	}

	// Check for data type nodes
	dtNodes := 0
	for _, n := range g.Nodes {
		if n.Kind == NodeDataType {
			dtNodes++
		}
	}
	if dtNodes != 2 {
		t.Errorf("expected 2 DataType nodes, got %d", dtNodes)
	}

	// Check for contains edges from module to data types
	for _, e := range g.Edges {
		if e.Kind == EdgeContains && strings.HasPrefix(e.To, "dt:") {
			if e.From != "mod:module1" {
				t.Errorf("data type edge should come from mod:module1, got %s", e.From)
			}
		}
	}
}

func TestAnalyze_IOContracts(t *testing.T) {
	sg := makeTestGraph(testModule{
		name: "module1",
		lang: "COBOL",
		ioContracts: []testIOContract{
			{name: "CUSTFILE", kind: ir.IOFile, direction: ir.IORead},
			{name: "ORDERDB", kind: ir.IODB, direction: ir.IOReadWrite},
		},
	})

	g := Analyze(sg)

	if g.Stats.IOCount != 2 {
		t.Errorf("expected 2 IO contracts, got %d", g.Stats.IOCount)
	}

	// Check for IO nodes with metadata
	ioNodes := 0
	for _, n := range g.Nodes {
		if n.Kind == NodeIO {
			ioNodes++
			if n.Metadata == nil {
				t.Error("IO node should have metadata")
			}
			if n.Metadata["kind"] == "" {
				t.Error("IO node metadata should have 'kind' field")
			}
			if n.Metadata["direction"] == "" {
				t.Error("IO node metadata should have 'direction' field")
			}
		}
	}
	if ioNodes != 2 {
		t.Errorf("expected 2 IO nodes, got %d", ioNodes)
	}

	// Check for IO edges
	ioEdges := countEdgesByKind(g, EdgeIO)
	if ioEdges != 2 {
		t.Errorf("expected 2 IO edges, got %d", ioEdges)
	}
}

func TestAnalyze_CallGraph(t *testing.T) {
	sg := makeTestGraph(
		testModule{
			name: "module1",
			lang: "COBOL",
			functions: []testFunc{
				{name: "main", calls: nil},
				{name: "helper", calls: nil},
			},
		},
	)

	// Add CallGraph edges
	sg.CallGraph = &ir.CallGraph{
		Edges: []ir.CallEdge{
			{Caller: "main", Callee: "helper"},
		},
	}

	g := Analyze(sg)

	// Should have call edge from main to helper
	found := false
	for _, e := range g.Edges {
		if e.Kind == EdgeCalls && strings.Contains(e.From, "main") && strings.Contains(e.To, "helper") {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected call graph edge main->helper not found")
	}
}

func TestAnalyze_ModuleDependencies(t *testing.T) {
	sg := makeTestGraph(
		testModule{
			name: "A",
			lang: "COBOL",
			functions: []testFunc{
				{name: "f1", calls: []string{"f2"}},
			},
		},
		testModule{
			name: "B",
			lang: "COBOL",
			functions: []testFunc{
				{name: "f2", calls: []string{"f3"}},
			},
		},
		testModule{
			name: "C",
			lang: "COBOL",
			functions: []testFunc{
				{name: "f3", calls: nil},
			},
		},
	)

	g := Analyze(sg)

	// Should have module dependencies: A->B, B->C
	moduleDeps := countEdgesByKind(g, EdgeDependsOn)
	if moduleDeps != 2 {
		t.Errorf("expected 2 module dependencies, got %d", moduleDeps)
	}

	// Check ModuleFanOut in stats
	if g.Stats.ModuleFanOut["A"] != 1 {
		t.Errorf("expected module A fan-out=1, got %d", g.Stats.ModuleFanOut["A"])
	}
	if g.Stats.ModuleFanOut["B"] != 1 {
		t.Errorf("expected module B fan-out=1, got %d", g.Stats.ModuleFanOut["B"])
	}
}

func TestAnalyze_CycleDetection(t *testing.T) {
	sg := makeTestGraph(
		testModule{
			name: "A",
			lang: "COBOL",
			functions: []testFunc{
				{name: "f1", calls: []string{"f2"}},
			},
		},
		testModule{
			name: "B",
			lang: "COBOL",
			functions: []testFunc{
				{name: "f2", calls: []string{"f3"}},
			},
		},
		testModule{
			name: "C",
			lang: "COBOL",
			functions: []testFunc{
				{name: "f3", calls: []string{"f1"}},
			},
		},
	)

	g := Analyze(sg)

	if len(g.Stats.CyclicDeps) == 0 {
		t.Error("expected cyclic dependency to be detected")
	}

	// Verify the cycle contains A, B, C
	if len(g.Stats.CyclicDeps) > 0 {
		cycle := g.Stats.CyclicDeps[0]
		cycleSet := make(map[string]bool)
		for _, mod := range cycle {
			cycleSet[mod] = true
		}
		if !cycleSet["A"] || !cycleSet["B"] || !cycleSet["C"] {
			t.Errorf("expected cycle to contain A, B, C; got %v", cycle)
		}
	}
}

func TestAnalyze_ConnectedComponents(t *testing.T) {
	// Two disconnected subgraphs
	sg := makeTestGraph(
		testModule{
			name: "A",
			lang: "COBOL",
			functions: []testFunc{
				{name: "f1", calls: []string{"f2"}},
			},
		},
		testModule{
			name: "B",
			lang: "COBOL",
			functions: []testFunc{
				{name: "f2", calls: nil},
			},
		},
		testModule{
			name: "C",
			lang: "COBOL",
			functions: []testFunc{
				{name: "f3", calls: []string{"f4"}},
			},
		},
		testModule{
			name: "D",
			lang: "COBOL",
			functions: []testFunc{
				{name: "f4", calls: nil},
			},
		},
	)

	g := Analyze(sg)

	// A->B and C->D are two separate components
	if g.Stats.ConnectedComponents != 2 {
		t.Errorf("expected 2 connected components, got %d", g.Stats.ConnectedComponents)
	}
}

func TestAnalyze_FanOutFanIn(t *testing.T) {
	sg := makeTestGraph(testModule{
		name: "module1",
		lang: "COBOL",
		functions: []testFunc{
			{name: "hub", calls: []string{"f1", "f2", "f3"}},
			{name: "f1", calls: nil},
			{name: "f2", calls: nil},
			{name: "f3", calls: nil},
		},
	})

	g := Analyze(sg)

	// hub has 3 outgoing call edges
	if g.Stats.MaxFanOut < 3 {
		t.Errorf("expected MaxFanOut >= 3, got %d", g.Stats.MaxFanOut)
	}

	// The module has the highest fan-out (contains all functions)
	if g.Stats.MaxFanOut < 4 {
		t.Errorf("expected MaxFanOut to be >= 4 (module contains 4 functions), got %d", g.Stats.MaxFanOut)
	}
}

func TestAnalyze_HotspotNode(t *testing.T) {
	sg := makeTestGraph(testModule{
		name: "module1",
		lang: "COBOL",
		functions: []testFunc{
			{name: "popular", calls: []string{"f1", "f2"}},
			{name: "f1", calls: nil},
			{name: "f2", calls: nil},
		},
	})

	g := Analyze(sg)

	// The module node should have the most outgoing edges (contains all functions)
	// or the popular function should have high fan-out
	if g.Stats.HotspotNode == "" {
		t.Error("expected HotspotNode to be set")
	}

	// Verify hotspot node exists in the graph
	found := false
	for _, n := range g.Nodes {
		if n.ID == g.Stats.HotspotNode {
			found = true
			break
		}
	}
	if !found {
		t.Errorf("HotspotNode %s not found in graph nodes", g.Stats.HotspotNode)
	}
}

func TestResolveCallTarget(t *testing.T) {
	sg := makeTestGraph(
		testModule{
			name: "module1",
			lang: "COBOL",
			functions: []testFunc{
				{name: "localFunc", calls: nil},
			},
		},
		testModule{
			name: "module2",
			lang: "COBOL",
			functions: []testFunc{
				{name: "remoteFunc", calls: nil},
			},
		},
	)

	// Test resolving a function that exists in another module
	target := resolveCallTarget("remoteFunc", "module1", sg)
	if target != "fn:module2.remoteFunc" {
		t.Errorf("expected fn:module2.remoteFunc, got %s", target)
	}

	// Test resolving a function that doesn't exist (defaults to current module)
	target = resolveCallTarget("unknownFunc", "module1", sg)
	if target != "fn:module1.unknownFunc" {
		t.Errorf("expected fn:module1.unknownFunc, got %s", target)
	}
}

// Export Tests

func TestExportDOT(t *testing.T) {
	sg := makeTestGraph(testModule{
		name: "module1",
		lang: "COBOL",
		functions: []testFunc{
			{name: "func1", calls: []string{"func2"}},
			{name: "func2", calls: nil},
		},
	})

	g := Analyze(sg)
	dot := ExportDOT(g)

	// Check for expected DOT elements
	if !strings.Contains(dot, "digraph dependencies") {
		t.Error("DOT output should contain 'digraph dependencies'")
	}
	if !strings.Contains(dot, "subgraph cluster_") {
		t.Error("DOT output should contain subgraph declarations")
	}
	if !strings.Contains(dot, "->") {
		t.Error("DOT output should contain edge declarations")
	}
	if !strings.Contains(dot, "module1") {
		t.Error("DOT output should contain module name")
	}
}

func TestExportMermaid(t *testing.T) {
	sg := makeTestGraph(testModule{
		name: "module1",
		lang: "COBOL",
		functions: []testFunc{
			{name: "func1", calls: []string{"func2"}},
			{name: "func2", calls: nil},
		},
	})

	g := Analyze(sg)
	mermaid := ExportMermaid(g)

	// Check for expected Mermaid elements
	if !strings.Contains(mermaid, "graph LR") {
		t.Error("Mermaid output should contain 'graph LR'")
	}
	if !strings.Contains(mermaid, "subgraph") {
		t.Error("Mermaid output should contain subgraph declarations")
	}
	if !strings.Contains(mermaid, "-->") || !strings.Contains(mermaid, "-.->" ) {
		t.Error("Mermaid output should contain arrow declarations")
	}
}

func TestExportJSON(t *testing.T) {
	sg := makeTestGraph(testModule{
		name: "module1",
		lang: "COBOL",
		functions: []testFunc{
			{name: "func1", calls: []string{"func2"}},
			{name: "func2", calls: nil},
		},
	})

	g := Analyze(sg)
	jsonBytes, err := ExportJSON(g)
	if err != nil {
		t.Fatalf("ExportJSON failed: %v", err)
	}

	// Unmarshal back to verify round-trip
	var g2 Graph
	if err := json.Unmarshal(jsonBytes, &g2); err != nil {
		t.Fatalf("JSON unmarshal failed: %v", err)
	}

	if len(g2.Nodes) != len(g.Nodes) {
		t.Errorf("expected %d nodes after roundtrip, got %d", len(g.Nodes), len(g2.Nodes))
	}
	if len(g2.Edges) != len(g.Edges) {
		t.Errorf("expected %d edges after roundtrip, got %d", len(g.Edges), len(g2.Edges))
	}
	if g2.Stats.TotalNodes != g.Stats.TotalNodes {
		t.Errorf("expected TotalNodes=%d after roundtrip, got %d", g.Stats.TotalNodes, g2.Stats.TotalNodes)
	}
}

func TestFormatStats(t *testing.T) {
	sg := makeTestGraph(
		testModule{
			name: "A",
			lang: "COBOL",
			functions: []testFunc{
				{name: "f1", calls: []string{"f2"}},
			},
		},
		testModule{
			name: "B",
			lang: "COBOL",
			functions: []testFunc{
				{name: "f2", calls: []string{"f1"}},
			},
		},
	)

	g := Analyze(sg)
	stats := FormatStats(g)

	// Check for expected labels
	expectedLabels := []string{
		"Dependency Graph Statistics",
		"Nodes:",
		"Modules:",
		"Functions:",
		"Edges:",
		"Max Fan-Out:",
		"Max Fan-In:",
		"Components:",
		"Module Dependencies:",
	}

	for _, label := range expectedLabels {
		if !strings.Contains(stats, label) {
			t.Errorf("stats should contain '%s'", label)
		}
	}

	// Should show cyclic dependency
	if !strings.Contains(stats, "Cyclic Dependencies:") {
		t.Error("stats should show cyclic dependencies")
	}
}

// Edge Cases

func TestAnalyze_DuplicateFunctions(t *testing.T) {
	// Same function name in different modules
	sg := makeTestGraph(
		testModule{
			name: "module1",
			lang: "COBOL",
			functions: []testFunc{
				{name: "process", calls: nil},
			},
		},
		testModule{
			name: "module2",
			lang: "COBOL",
			functions: []testFunc{
				{name: "process", calls: nil},
			},
		},
	)

	g := Analyze(sg)

	// Should have 2 distinct function nodes
	if g.Stats.FunctionCount != 2 {
		t.Errorf("expected 2 functions with same name in different modules, got %d", g.Stats.FunctionCount)
	}

	// Verify both nodes exist with different IDs
	foundMod1 := false
	foundMod2 := false
	for _, n := range g.Nodes {
		if n.Kind == NodeFunction && n.Name == "process" {
			if n.Module == "module1" {
				foundMod1 = true
			}
			if n.Module == "module2" {
				foundMod2 = true
			}
		}
	}
	if !foundMod1 || !foundMod2 {
		t.Error("both duplicate functions should be represented as separate nodes")
	}
}

func TestAnalyze_SelfCalls(t *testing.T) {
	// Function that calls itself (recursion)
	sg := makeTestGraph(testModule{
		name: "module1",
		lang: "COBOL",
		functions: []testFunc{
			{name: "recursive", calls: []string{"recursive"}},
		},
	})

	g := Analyze(sg)

	// Should have a self-referencing edge
	found := false
	for _, e := range g.Edges {
		if e.Kind == EdgeCalls && e.From == "fn:module1.recursive" && e.To == "fn:module1.recursive" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected self-referencing call edge for recursive function")
	}
}

func TestAnalyze_NilCallGraph(t *testing.T) {
	sg := makeTestGraph(testModule{
		name: "module1",
		lang: "COBOL",
		functions: []testFunc{
			{name: "func1", calls: []string{"func2"}},
			{name: "func2", calls: nil},
		},
	})

	// Explicitly set CallGraph to nil
	sg.CallGraph = nil

	// Should not panic
	g := Analyze(sg)

	if g == nil {
		t.Error("Analyze should return a valid graph even with nil CallGraph")
	}
	if g.Stats.FunctionCount != 2 {
		t.Errorf("expected 2 functions, got %d", g.Stats.FunctionCount)
	}
}

func TestAnalyze_EmptyModule(t *testing.T) {
	// Module with no functions, data types, or IO contracts
	sg := makeTestGraph(testModule{
		name: "empty",
		lang: "COBOL",
	})

	g := Analyze(sg)

	// Should have 1 module node but nothing else
	if g.Stats.ModuleCount != 1 {
		t.Errorf("expected 1 module, got %d", g.Stats.ModuleCount)
	}
	if g.Stats.FunctionCount != 0 {
		t.Errorf("expected 0 functions, got %d", g.Stats.FunctionCount)
	}
	if g.Stats.TotalNodes != 1 {
		t.Errorf("expected 1 node (module only), got %d", g.Stats.TotalNodes)
	}
}

func TestAnalyze_NoCycles(t *testing.T) {
	// Linear dependency chain with no cycles
	sg := makeTestGraph(
		testModule{
			name: "A",
			lang: "COBOL",
			functions: []testFunc{
				{name: "f1", calls: []string{"f2"}},
			},
		},
		testModule{
			name: "B",
			lang: "COBOL",
			functions: []testFunc{
				{name: "f2", calls: []string{"f3"}},
			},
		},
		testModule{
			name: "C",
			lang: "COBOL",
			functions: []testFunc{
				{name: "f3", calls: nil},
			},
		},
	)

	g := Analyze(sg)

	if len(g.Stats.CyclicDeps) != 0 {
		t.Errorf("expected no cyclic dependencies, got %d cycles", len(g.Stats.CyclicDeps))
	}
}

// Helper functions

func countEdgesByKind(g *Graph, kind EdgeKind) int {
	count := 0
	for _, e := range g.Edges {
		if e.Kind == kind {
			count++
		}
	}
	return count
}
