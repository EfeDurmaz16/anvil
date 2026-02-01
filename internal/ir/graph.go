package ir

// SemanticGraph is the central intermediate representation produced by source
// plugins and consumed by agents and target plugins.
type SemanticGraph struct {
	Modules      []*Module      `json:"modules"`
	CallGraph    *CallGraph     `json:"call_graph,omitempty"`
	DataTypes    []*DataType    `json:"data_types,omitempty"`
	BusinessRules []*BusinessRule `json:"business_rules,omitempty"`
	Metadata     map[string]string `json:"metadata,omitempty"`
}

// Module represents a single source file / compilation unit.
type Module struct {
	Name       string      `json:"name"`
	Path       string      `json:"path"`
	Language   string      `json:"language"`
	Functions  []*Function `json:"functions,omitempty"`
	DataTypes  []*DataType `json:"data_types,omitempty"`
	IOContracts []*IOContract `json:"io_contracts,omitempty"`
	Metadata   map[string]string `json:"metadata,omitempty"`
}

// Function represents a callable unit (paragraph, section, method, etc.).
type Function struct {
	Name       string            `json:"name"`
	Parameters []*Parameter      `json:"parameters,omitempty"`
	Returns    *DataType         `json:"returns,omitempty"`
	Body       string            `json:"body,omitempty"`
	CFG        *CFG              `json:"cfg,omitempty"`
	Calls      []string          `json:"calls,omitempty"`
	Metadata   map[string]string `json:"metadata,omitempty"`
}

// Parameter describes a function input.
type Parameter struct {
	Name     string   `json:"name"`
	Type     *DataType `json:"type"`
	Optional bool     `json:"optional,omitempty"`
}

// DataType describes a type in the IR.
type DataType struct {
	Name       string      `json:"name"`
	Kind       TypeKind    `json:"kind"`
	Size       int         `json:"size,omitempty"`
	Scale      int         `json:"scale,omitempty"`
	Fields     []*DataType `json:"fields,omitempty"`
	ElementType *DataType  `json:"element_type,omitempty"`
	Metadata   map[string]string `json:"metadata,omitempty"`
}

// TypeKind classifies data types.
type TypeKind string

const (
	TypeString  TypeKind = "string"
	TypeInteger TypeKind = "integer"
	TypeDecimal TypeKind = "decimal"
	TypeBoolean TypeKind = "boolean"
	TypeStruct  TypeKind = "struct"
	TypeArray   TypeKind = "array"
	TypeUnknown TypeKind = "unknown"
)

// BusinessRule captures an extracted business rule from source code.
type BusinessRule struct {
	ID          string   `json:"id"`
	Description string   `json:"description"`
	SourceRef   string   `json:"source_ref"`
	Confidence  float64  `json:"confidence"`
	Tags        []string `json:"tags,omitempty"`
}

// IOContract describes external I/O (files, DB, screens, etc.).
type IOContract struct {
	Name      string            `json:"name"`
	Kind      IOKind            `json:"kind"`
	Direction IODirection       `json:"direction"`
	Schema    *DataType         `json:"schema,omitempty"`
	Metadata  map[string]string `json:"metadata,omitempty"`
}

// IOKind classifies I/O resources.
type IOKind string

const (
	IOFile   IOKind = "file"
	IODB     IOKind = "database"
	IOScreen IOKind = "screen"
	IOQueue  IOKind = "queue"
)

// IODirection indicates read, write, or both.
type IODirection string

const (
	IORead      IODirection = "read"
	IOWrite     IODirection = "write"
	IOReadWrite IODirection = "readwrite"
)

// CallGraph maps function-level call relationships.
type CallGraph struct {
	Edges []CallEdge `json:"edges"`
}

// CallEdge represents a caller → callee relationship.
type CallEdge struct {
	Caller string `json:"caller"`
	Callee string `json:"callee"`
}

// CFG is a simplified control-flow graph for a single function.
type CFG struct {
	Blocks []*BasicBlock `json:"blocks"`
}

// BasicBlock is a straight-line sequence of statements.
type BasicBlock struct {
	ID         string   `json:"id"`
	Statements []string `json:"statements,omitempty"`
	Successors []string `json:"successors,omitempty"`
}
