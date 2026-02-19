package depgraph

import (
	"encoding/json"
	"fmt"
	"strings"
)

// ExportDOT generates a Graphviz DOT representation of the graph.
func ExportDOT(g *Graph) string {
	var b strings.Builder
	b.WriteString("digraph dependencies {\n")
	b.WriteString("  rankdir=LR;\n")
	b.WriteString("  node [fontname=\"Helvetica\"];\n")
	b.WriteString("  edge [fontname=\"Helvetica\" fontsize=10];\n\n")

	// Group nodes by module using subgraphs
	modules := make(map[string][]Node)
	for _, n := range g.Nodes {
		if n.Kind != NodeModule {
			modules[n.Module] = append(modules[n.Module], n)
		}
	}

	for modName, nodes := range modules {
		b.WriteString(fmt.Sprintf("  subgraph cluster_%s {\n", sanitizeDOTID(modName)))
		b.WriteString(fmt.Sprintf("    label=\"%s\";\n", modName))
		b.WriteString("    style=dashed;\n")
		b.WriteString("    color=\"#58a6ff\";\n")
		for _, n := range nodes {
			shape := nodeShape(n.Kind)
			color := nodeColor(n.Kind)
			b.WriteString(fmt.Sprintf("    \"%s\" [label=\"%s\" shape=%s style=filled fillcolor=\"%s\"];\n",
				n.ID, n.Name, shape, color))
		}
		b.WriteString("  }\n\n")
	}

	// Add edges
	for _, e := range g.Edges {
		style := edgeStyle(e.Kind)
		color := edgeColor(e.Kind)
		label := ""
		if e.Label != "" {
			label = fmt.Sprintf(" label=\"%s\"", e.Label)
		}
		b.WriteString(fmt.Sprintf("  \"%s\" -> \"%s\" [style=%s color=\"%s\"%s];\n",
			e.From, e.To, style, color, label))
	}

	b.WriteString("}\n")
	return b.String()
}

// ExportMermaid generates a Mermaid diagram of the graph.
func ExportMermaid(g *Graph) string {
	var b strings.Builder
	b.WriteString("graph LR\n")

	// Add nodes grouped by module
	modules := make(map[string][]Node)
	for _, n := range g.Nodes {
		modules[n.Module] = append(modules[n.Module], n)
	}

	for modName, nodes := range modules {
		b.WriteString(fmt.Sprintf("  subgraph %s\n", sanitizeMermaidID(modName)))
		for _, n := range nodes {
			mermaidShape := mermaidNodeShape(n)
			b.WriteString(fmt.Sprintf("    %s%s\n", sanitizeMermaidID(n.ID), mermaidShape))
		}
		b.WriteString("  end\n")
	}

	// Add edges
	for _, e := range g.Edges {
		arrow := mermaidArrow(e.Kind)
		label := ""
		if e.Label != "" {
			label = "|" + e.Label + "|"
		}
		b.WriteString(fmt.Sprintf("  %s %s%s %s\n",
			sanitizeMermaidID(e.From), arrow, label, sanitizeMermaidID(e.To)))
	}

	return b.String()
}

// ExportJSON serializes the graph to JSON.
func ExportJSON(g *Graph) ([]byte, error) {
	return json.MarshalIndent(g, "", "  ")
}

// FormatStats returns a human-readable summary of graph statistics.
func FormatStats(g *Graph) string {
	var b strings.Builder
	b.WriteString("Dependency Graph Statistics\n")
	b.WriteString("==========================\n\n")
	b.WriteString(fmt.Sprintf("Nodes:       %d total\n", g.Stats.TotalNodes))
	b.WriteString(fmt.Sprintf("  Modules:   %d\n", g.Stats.ModuleCount))
	b.WriteString(fmt.Sprintf("  Functions: %d\n", g.Stats.FunctionCount))
	b.WriteString(fmt.Sprintf("  DataTypes: %d\n", g.Stats.DataTypeCount))
	b.WriteString(fmt.Sprintf("  I/O:       %d\n", g.Stats.IOCount))
	b.WriteString(fmt.Sprintf("Edges:       %d total\n", g.Stats.TotalEdges))
	b.WriteString(fmt.Sprintf("Max Fan-Out: %d (%s)\n", g.Stats.MaxFanOut, g.Stats.HotspotNode))
	b.WriteString(fmt.Sprintf("Max Fan-In:  %d\n", g.Stats.MaxFanIn))
	b.WriteString(fmt.Sprintf("Components:  %d\n", g.Stats.ConnectedComponents))

	if len(g.Stats.CyclicDeps) > 0 {
		b.WriteString(fmt.Sprintf("\nCyclic Dependencies: %d\n", len(g.Stats.CyclicDeps)))
		for i, cycle := range g.Stats.CyclicDeps {
			b.WriteString(fmt.Sprintf("  %d: %s\n", i+1, strings.Join(cycle, " -> ")))
		}
	}

	if len(g.Stats.ModuleFanOut) > 0 {
		b.WriteString("\nModule Dependencies:\n")
		for mod, count := range g.Stats.ModuleFanOut {
			b.WriteString(fmt.Sprintf("  %s: %d outgoing\n", mod, count))
		}
	}

	return b.String()
}

func sanitizeDOTID(s string) string {
	return strings.Map(func(r rune) rune {
		if (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9') || r == '_' {
			return r
		}
		return '_'
	}, s)
}

func sanitizeMermaidID(s string) string {
	return strings.Map(func(r rune) rune {
		if (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9') || r == '_' {
			return r
		}
		return '_'
	}, s)
}

func nodeShape(kind NodeKind) string {
	switch kind {
	case NodeModule:
		return "box3d"
	case NodeFunction:
		return "box"
	case NodeDataType:
		return "ellipse"
	case NodeIO:
		return "diamond"
	default:
		return "box"
	}
}

func nodeColor(kind NodeKind) string {
	switch kind {
	case NodeModule:
		return "#1f6feb"
	case NodeFunction:
		return "#238636"
	case NodeDataType:
		return "#8957e5"
	case NodeIO:
		return "#d29922"
	default:
		return "#30363d"
	}
}

func edgeStyle(kind EdgeKind) string {
	switch kind {
	case EdgeCalls:
		return "solid"
	case EdgeContains:
		return "dashed"
	case EdgeUses:
		return "dotted"
	case EdgeIO:
		return "bold"
	case EdgeDependsOn:
		return "bold"
	default:
		return "solid"
	}
}

func edgeColor(kind EdgeKind) string {
	switch kind {
	case EdgeCalls:
		return "#3fb950"
	case EdgeContains:
		return "#8b949e"
	case EdgeUses:
		return "#8957e5"
	case EdgeIO:
		return "#d29922"
	case EdgeDependsOn:
		return "#f85149"
	default:
		return "#c9d1d9"
	}
}

func mermaidNodeShape(n Node) string {
	switch n.Kind {
	case NodeModule:
		return fmt.Sprintf("[[\"%s\"]]", n.Name)
	case NodeFunction:
		return fmt.Sprintf("[\"%s\"]", n.Name)
	case NodeDataType:
		return fmt.Sprintf("([\"%s\"])", n.Name)
	case NodeIO:
		return fmt.Sprintf("{\"%s\"}", n.Name)
	default:
		return fmt.Sprintf("[\"%s\"]", n.Name)
	}
}

func mermaidArrow(kind EdgeKind) string {
	switch kind {
	case EdgeCalls:
		return "-->"
	case EdgeContains:
		return "-.->"
	case EdgeUses:
		return "-..->"
	case EdgeIO:
		return "==>"
	case EdgeDependsOn:
		return "===>"
	default:
		return "-->"
	}
}
