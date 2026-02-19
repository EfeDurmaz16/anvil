package depgraph

import (
	"fmt"
	"sort"

	"github.com/efebarandurmaz/anvil/internal/ir"
)

// Analyze builds a dependency graph from a SemanticGraph.
func Analyze(sg *ir.SemanticGraph) *Graph {
	g := &Graph{}

	nodeMap := make(map[string]bool) // track existing node IDs

	// 1. Add module nodes
	for _, mod := range sg.Modules {
		modID := "mod:" + mod.Name
		if !nodeMap[modID] {
			g.Nodes = append(g.Nodes, Node{
				ID:       modID,
				Name:     mod.Name,
				Kind:     NodeModule,
				Module:   mod.Name,
				Language: mod.Language,
			})
			nodeMap[modID] = true
		}

		// 2. Add function nodes + contains edges
		for _, fn := range mod.Functions {
			fnID := fmt.Sprintf("fn:%s.%s", mod.Name, fn.Name)
			if !nodeMap[fnID] {
				g.Nodes = append(g.Nodes, Node{
					ID:       fnID,
					Name:     fn.Name,
					Kind:     NodeFunction,
					Module:   mod.Name,
					Language: mod.Language,
				})
				nodeMap[fnID] = true
			}
			g.Edges = append(g.Edges, Edge{
				From: modID,
				To:   fnID,
				Kind: EdgeContains,
			})

			// 3. Add call edges from fn.Calls
			for _, callee := range fn.Calls {
				calleeID := resolveCallTarget(callee, mod.Name, sg)
				g.Edges = append(g.Edges, Edge{
					From: fnID,
					To:   calleeID,
					Kind: EdgeCalls,
				})
				// Ensure target node exists
				if !nodeMap[calleeID] {
					g.Nodes = append(g.Nodes, Node{
						ID:     calleeID,
						Name:   callee,
						Kind:   NodeFunction,
						Module: extractModule(calleeID),
					})
					nodeMap[calleeID] = true
				}
			}
		}

		// 4. Add data type nodes + uses edges
		for _, dt := range mod.DataTypes {
			dtID := fmt.Sprintf("dt:%s.%s", mod.Name, dt.Name)
			if !nodeMap[dtID] {
				g.Nodes = append(g.Nodes, Node{
					ID:     dtID,
					Name:   dt.Name,
					Kind:   NodeDataType,
					Module: mod.Name,
				})
				nodeMap[dtID] = true
			}
			g.Edges = append(g.Edges, Edge{
				From: modID,
				To:   dtID,
				Kind: EdgeContains,
			})
		}

		// 5. Add IO contract nodes + io edges
		for _, io := range mod.IOContracts {
			ioID := fmt.Sprintf("io:%s.%s", mod.Name, io.Name)
			if !nodeMap[ioID] {
				g.Nodes = append(g.Nodes, Node{
					ID:     ioID,
					Name:   io.Name,
					Kind:   NodeIO,
					Module: mod.Name,
					Metadata: map[string]string{
						"kind":      string(io.Kind),
						"direction": string(io.Direction),
					},
				})
				nodeMap[ioID] = true
			}
			g.Edges = append(g.Edges, Edge{
				From: modID,
				To:   ioID,
				Kind: EdgeIO,
			})
		}
	}

	// 6. Add CallGraph edges if present
	if sg.CallGraph != nil {
		for _, edge := range sg.CallGraph.Edges {
			callerID := resolveGlobalName(edge.Caller, sg)
			calleeID := resolveGlobalName(edge.Callee, sg)
			// Only add if not already present
			if !hasEdge(g, callerID, calleeID, EdgeCalls) {
				g.Edges = append(g.Edges, Edge{
					From: callerID,
					To:   calleeID,
					Kind: EdgeCalls,
				})
			}
		}
	}

	// 7. Compute module-level dependencies
	g.addModuleDependencies()

	// 8. Compute stats
	g.computeStats()

	return g
}

// resolveCallTarget resolves a callee name to a full node ID
func resolveCallTarget(callee, currentModule string, sg *ir.SemanticGraph) string {
	// Check if callee exists in any module
	for _, mod := range sg.Modules {
		for _, fn := range mod.Functions {
			if fn.Name == callee {
				return fmt.Sprintf("fn:%s.%s", mod.Name, fn.Name)
			}
		}
	}
	// Default to current module
	return fmt.Sprintf("fn:%s.%s", currentModule, callee)
}

// resolveGlobalName resolves a name from the call graph
func resolveGlobalName(name string, sg *ir.SemanticGraph) string {
	for _, mod := range sg.Modules {
		for _, fn := range mod.Functions {
			if fn.Name == name {
				return fmt.Sprintf("fn:%s.%s", mod.Name, fn.Name)
			}
		}
	}
	return "fn:unknown." + name
}

// extractModule extracts module name from a node ID like "fn:module.func"
func extractModule(nodeID string) string {
	// Strip prefix (fn:, dt:, io:, mod:)
	for _, prefix := range []string{"fn:", "dt:", "io:", "mod:"} {
		if len(nodeID) > len(prefix) && nodeID[:len(prefix)] == prefix {
			rest := nodeID[len(prefix):]
			for i, c := range rest {
				if c == '.' {
					return rest[:i]
				}
			}
			return rest
		}
	}
	return ""
}

func hasEdge(g *Graph, from, to string, kind EdgeKind) bool {
	for _, e := range g.Edges {
		if e.From == from && e.To == to && e.Kind == kind {
			return true
		}
	}
	return false
}

// addModuleDependencies computes module-to-module edges from function calls
func (g *Graph) addModuleDependencies() {
	modDeps := make(map[string]map[string]bool)
	for _, e := range g.Edges {
		if e.Kind == EdgeCalls {
			fromMod := extractModule(e.From)
			toMod := extractModule(e.To)
			if fromMod != "" && toMod != "" && fromMod != toMod {
				if modDeps[fromMod] == nil {
					modDeps[fromMod] = make(map[string]bool)
				}
				modDeps[fromMod][toMod] = true
			}
		}
	}
	for from, tos := range modDeps {
		for to := range tos {
			g.Edges = append(g.Edges, Edge{
				From: "mod:" + from,
				To:   "mod:" + to,
				Kind: EdgeDependsOn,
			})
		}
	}
}

// computeStats computes graph metrics
func (g *Graph) computeStats() {
	g.Stats.TotalNodes = len(g.Nodes)
	g.Stats.TotalEdges = len(g.Edges)

	fanOut := make(map[string]int)
	fanIn := make(map[string]int)
	g.Stats.ModuleFanOut = make(map[string]int)

	for _, n := range g.Nodes {
		switch n.Kind {
		case NodeModule:
			g.Stats.ModuleCount++
		case NodeFunction:
			g.Stats.FunctionCount++
		case NodeDataType:
			g.Stats.DataTypeCount++
		case NodeIO:
			g.Stats.IOCount++
		}
	}

	for _, e := range g.Edges {
		fanOut[e.From]++
		fanIn[e.To]++
		if e.Kind == EdgeDependsOn {
			fromMod := extractModule(e.From)
			g.Stats.ModuleFanOut[fromMod]++
		}
	}

	for id, count := range fanOut {
		if count > g.Stats.MaxFanOut {
			g.Stats.MaxFanOut = count
			g.Stats.HotspotNode = id
		}
	}
	for _, count := range fanIn {
		if count > g.Stats.MaxFanIn {
			g.Stats.MaxFanIn = count
		}
	}

	// Detect connected components using union-find
	g.Stats.ConnectedComponents = g.countComponents()

	// Detect cycles
	g.Stats.CyclicDeps = g.detectCycles()
}

// countComponents counts connected components via union-find
func (g *Graph) countComponents() int {
	parent := make(map[string]string)
	var find func(string) string
	find = func(x string) string {
		if parent[x] == "" {
			parent[x] = x
		}
		if parent[x] != x {
			parent[x] = find(parent[x])
		}
		return parent[x]
	}
	union := func(a, b string) {
		fa, fb := find(a), find(b)
		if fa != fb {
			parent[fa] = fb
		}
	}

	for _, n := range g.Nodes {
		find(n.ID)
	}
	for _, e := range g.Edges {
		union(e.From, e.To)
	}

	roots := make(map[string]bool)
	for _, n := range g.Nodes {
		roots[find(n.ID)] = true
	}
	return len(roots)
}

// detectCycles finds cycles using DFS on module-level dependency edges
func (g *Graph) detectCycles() [][]string {
	// Build adjacency list for module-level deps only
	adj := make(map[string][]string)
	modules := make(map[string]bool)

	for _, e := range g.Edges {
		if e.Kind == EdgeDependsOn {
			from := extractModule(e.From)
			to := extractModule(e.To)
			adj[from] = append(adj[from], to)
			modules[from] = true
			modules[to] = true
		}
	}

	var cycles [][]string
	visited := make(map[string]int) // 0=unvisited, 1=in-progress, 2=done
	path := make([]string, 0)

	var dfs func(node string)
	dfs = func(node string) {
		if visited[node] == 2 {
			return
		}
		if visited[node] == 1 {
			// Found cycle - extract it
			cycle := make([]string, 0)
			for i := len(path) - 1; i >= 0; i-- {
				cycle = append(cycle, path[i])
				if path[i] == node {
					break
				}
			}
			// Reverse the cycle
			for i, j := 0, len(cycle)-1; i < j; i, j = i+1, j-1 {
				cycle[i], cycle[j] = cycle[j], cycle[i]
			}
			cycles = append(cycles, cycle)
			return
		}
		visited[node] = 1
		path = append(path, node)
		for _, next := range adj[node] {
			dfs(next)
		}
		path = path[:len(path)-1]
		visited[node] = 2
	}

	// Sort modules for deterministic output
	sortedModules := make([]string, 0, len(modules))
	for m := range modules {
		sortedModules = append(sortedModules, m)
	}
	sort.Strings(sortedModules)

	for _, m := range sortedModules {
		if visited[m] == 0 {
			dfs(m)
		}
	}

	return cycles
}
