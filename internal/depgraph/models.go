package depgraph

// Node represents a node in the dependency graph
type Node struct {
	ID       string            `json:"id"`
	Name     string            `json:"name"`
	Kind     NodeKind          `json:"kind"`      // module, function, datatype, io
	Module   string            `json:"module"`    // parent module name
	Language string            `json:"language"`  // source language
	Metadata map[string]string `json:"metadata,omitempty"`
}

// NodeKind classifies graph nodes
type NodeKind string

const (
	NodeModule   NodeKind = "module"
	NodeFunction NodeKind = "function"
	NodeDataType NodeKind = "datatype"
	NodeIO       NodeKind = "io"
)

// Edge represents a directed edge between two nodes
type Edge struct {
	From   string   `json:"from"`
	To     string   `json:"to"`
	Kind   EdgeKind `json:"kind"`
	Weight int      `json:"weight,omitempty"` // call frequency or importance
	Label  string   `json:"label,omitempty"`
}

// EdgeKind classifies relationships
type EdgeKind string

const (
	EdgeCalls     EdgeKind = "calls"      // function calls function
	EdgeContains  EdgeKind = "contains"   // module contains function
	EdgeUses      EdgeKind = "uses"       // function uses datatype
	EdgeIO        EdgeKind = "io"         // function performs I/O
	EdgeDependsOn EdgeKind = "depends_on" // module depends on module
)

// Graph is the full dependency graph
type Graph struct {
	Nodes []Node     `json:"nodes"`
	Edges []Edge     `json:"edges"`
	Stats GraphStats `json:"stats"`
}

// GraphStats holds computed metrics about the graph
type GraphStats struct {
	TotalNodes          int            `json:"total_nodes"`
	TotalEdges          int            `json:"total_edges"`
	ModuleCount         int            `json:"module_count"`
	FunctionCount       int            `json:"function_count"`
	DataTypeCount       int            `json:"data_type_count"`
	IOCount             int            `json:"io_count"`
	MaxFanOut           int            `json:"max_fan_out"`        // most outgoing edges
	MaxFanIn            int            `json:"max_fan_in"`         // most incoming edges
	HotspotNode         string         `json:"hotspot_node"`       // node with most connections
	ConnectedComponents int            `json:"connected_components"`
	CyclicDeps          [][]string     `json:"cyclic_deps,omitempty"`
	ModuleFanOut        map[string]int `json:"module_fan_out"` // per-module outgoing edge count
}
