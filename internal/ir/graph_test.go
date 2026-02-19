package ir

import (
	"encoding/json"
	"testing"
)

func TestSemanticGraph_Creation(t *testing.T) {
	sg := &SemanticGraph{
		Modules: []*Module{
			{Name: "test-module", Path: "/test", Language: "COBOL"},
		},
		CallGraph: &CallGraph{
			Edges: []CallEdge{{Caller: "A", Callee: "B"}},
		},
		DataTypes: []*DataType{
			{Name: "TestType", Kind: TypeString},
		},
		BusinessRules: []*BusinessRule{
			{ID: "BR1", Description: "Test rule", Confidence: 0.9},
		},
		Metadata: map[string]string{"version": "1.0"},
	}

	if len(sg.Modules) != 1 {
		t.Errorf("expected 1 module, got %d", len(sg.Modules))
	}
	if sg.CallGraph == nil {
		t.Error("expected CallGraph to be set")
	}
	if len(sg.DataTypes) != 1 {
		t.Errorf("expected 1 data type, got %d", len(sg.DataTypes))
	}
	if len(sg.BusinessRules) != 1 {
		t.Errorf("expected 1 business rule, got %d", len(sg.BusinessRules))
	}
	if sg.Metadata["version"] != "1.0" {
		t.Errorf("expected metadata version 1.0, got %s", sg.Metadata["version"])
	}
}

func TestSemanticGraph_JSONRoundTrip(t *testing.T) {
	original := &SemanticGraph{
		Modules: []*Module{
			{
				Name:     "TestModule",
				Path:     "/test/path",
				Language: "COBOL",
			},
		},
		Metadata: map[string]string{"key": "value"},
	}

	data, err := json.Marshal(original)
	if err != nil {
		t.Fatalf("failed to marshal: %v", err)
	}

	var decoded SemanticGraph
	if err := json.Unmarshal(data, &decoded); err != nil {
		t.Fatalf("failed to unmarshal: %v", err)
	}

	if len(decoded.Modules) != 1 {
		t.Errorf("expected 1 module, got %d", len(decoded.Modules))
	}
	if decoded.Modules[0].Name != "TestModule" {
		t.Errorf("expected module name TestModule, got %s", decoded.Modules[0].Name)
	}
	if decoded.Metadata["key"] != "value" {
		t.Errorf("expected metadata key=value, got %s", decoded.Metadata["key"])
	}
}

func TestSemanticGraph_EmptyFields(t *testing.T) {
	sg := &SemanticGraph{}

	data, err := json.Marshal(sg)
	if err != nil {
		t.Fatalf("failed to marshal empty graph: %v", err)
	}

	var decoded SemanticGraph
	if err := json.Unmarshal(data, &decoded); err != nil {
		t.Fatalf("failed to unmarshal: %v", err)
	}

	if decoded.Modules != nil {
		t.Errorf("expected nil modules, got %v", decoded.Modules)
	}
	if decoded.CallGraph != nil {
		t.Errorf("expected nil CallGraph, got %v", decoded.CallGraph)
	}
}

func TestModule_WithAllFields(t *testing.T) {
	mod := &Module{
		Name:     "CUSTPROC",
		Path:     "/src/CUSTPROC.cbl",
		Language: "COBOL",
		Functions: []*Function{
			{Name: "PROCESS-CUSTOMER", Body: "MOVE X TO Y."},
		},
		DataTypes: []*DataType{
			{Name: "CUSTOMER-REC", Kind: TypeStruct},
		},
		IOContracts: []*IOContract{
			{Name: "CUSTFILE", Kind: IOFile, Direction: IORead},
		},
		Metadata: map[string]string{"lines": "500"},
	}

	if mod.Name != "CUSTPROC" {
		t.Errorf("expected name CUSTPROC, got %s", mod.Name)
	}
	if len(mod.Functions) != 1 {
		t.Errorf("expected 1 function, got %d", len(mod.Functions))
	}
	if len(mod.DataTypes) != 1 {
		t.Errorf("expected 1 data type, got %d", len(mod.DataTypes))
	}
	if len(mod.IOContracts) != 1 {
		t.Errorf("expected 1 IO contract, got %d", len(mod.IOContracts))
	}
	if mod.Metadata["lines"] != "500" {
		t.Errorf("expected metadata lines=500, got %s", mod.Metadata["lines"])
	}
}

func TestFunction_JSONRoundTrip(t *testing.T) {
	original := &Function{
		Name: "VALIDATE-INPUT",
		Parameters: []*Parameter{
			{Name: "input", Type: &DataType{Name: "string", Kind: TypeString}, Optional: false},
			{Name: "flags", Type: &DataType{Name: "int", Kind: TypeInteger}, Optional: true},
		},
		Returns: &DataType{Name: "bool", Kind: TypeBoolean},
		Body:    "IF INPUT = SPACES THEN RETURN FALSE.",
		Calls:   []string{"LOG-ERROR", "FORMAT-MESSAGE"},
		Metadata: map[string]string{"complexity": "high"},
	}

	data, err := json.Marshal(original)
	if err != nil {
		t.Fatalf("failed to marshal: %v", err)
	}

	var decoded Function
	if err := json.Unmarshal(data, &decoded); err != nil {
		t.Fatalf("failed to unmarshal: %v", err)
	}

	if decoded.Name != "VALIDATE-INPUT" {
		t.Errorf("expected name VALIDATE-INPUT, got %s", decoded.Name)
	}
	if len(decoded.Parameters) != 2 {
		t.Errorf("expected 2 parameters, got %d", len(decoded.Parameters))
	}
	if decoded.Parameters[1].Optional != true {
		t.Error("expected second parameter to be optional")
	}
	if decoded.Returns.Kind != TypeBoolean {
		t.Errorf("expected return type boolean, got %s", decoded.Returns.Kind)
	}
	if len(decoded.Calls) != 2 {
		t.Errorf("expected 2 calls, got %d", len(decoded.Calls))
	}
}

func TestFunction_WithCFG(t *testing.T) {
	fn := &Function{
		Name: "CALC-TOTAL",
		CFG: &CFG{
			Blocks: []*BasicBlock{
				{ID: "entry", Statements: []string{"MOVE 0 TO TOTAL"}, Successors: []string{"loop"}},
				{ID: "loop", Statements: []string{"ADD ITEM TO TOTAL"}, Successors: []string{"exit"}},
				{ID: "exit", Statements: []string{"DISPLAY TOTAL"}},
			},
		},
	}

	if fn.CFG == nil {
		t.Fatal("expected CFG to be set")
	}
	if len(fn.CFG.Blocks) != 3 {
		t.Errorf("expected 3 blocks, got %d", len(fn.CFG.Blocks))
	}
	if fn.CFG.Blocks[0].ID != "entry" {
		t.Errorf("expected first block ID 'entry', got %s", fn.CFG.Blocks[0].ID)
	}
	if len(fn.CFG.Blocks[0].Successors) != 1 {
		t.Errorf("expected 1 successor, got %d", len(fn.CFG.Blocks[0].Successors))
	}
}

func TestParameter_OptionalFlag(t *testing.T) {
	required := &Parameter{
		Name: "required-param",
		Type: &DataType{Name: "string", Kind: TypeString},
		Optional: false,
	}

	optional := &Parameter{
		Name: "optional-param",
		Type: &DataType{Name: "int", Kind: TypeInteger},
		Optional: true,
	}

	if required.Optional {
		t.Error("expected required parameter to have Optional=false")
	}
	if !optional.Optional {
		t.Error("expected optional parameter to have Optional=true")
	}
}

func TestDataType_AllTypeKinds(t *testing.T) {
	tests := []struct {
		name string
		kind TypeKind
		want string
	}{
		{"string type", TypeString, "string"},
		{"integer type", TypeInteger, "integer"},
		{"decimal type", TypeDecimal, "decimal"},
		{"boolean type", TypeBoolean, "boolean"},
		{"struct type", TypeStruct, "struct"},
		{"array type", TypeArray, "array"},
		{"unknown type", TypeUnknown, "unknown"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			dt := &DataType{Name: tt.name, Kind: tt.kind}
			if string(dt.Kind) != tt.want {
				t.Errorf("expected kind %s, got %s", tt.want, dt.Kind)
			}
		})
	}
}

func TestDataType_NestedStruct(t *testing.T) {
	customerType := &DataType{
		Name: "CUSTOMER",
		Kind: TypeStruct,
		Fields: []*DataType{
			{Name: "ID", Kind: TypeInteger, Size: 10},
			{Name: "NAME", Kind: TypeString, Size: 50},
			{Name: "BALANCE", Kind: TypeDecimal, Size: 15, Scale: 2},
			{Name: "ACTIVE", Kind: TypeBoolean},
		},
	}

	if customerType.Kind != TypeStruct {
		t.Errorf("expected struct type, got %s", customerType.Kind)
	}
	if len(customerType.Fields) != 4 {
		t.Errorf("expected 4 fields, got %d", len(customerType.Fields))
	}
	if customerType.Fields[2].Scale != 2 {
		t.Errorf("expected scale 2 for BALANCE, got %d", customerType.Fields[2].Scale)
	}
}

func TestDataType_ArrayWithElementType(t *testing.T) {
	arrayType := &DataType{
		Name: "CUSTOMER-ARRAY",
		Kind: TypeArray,
		ElementType: &DataType{
			Name: "CUSTOMER",
			Kind: TypeStruct,
			Fields: []*DataType{
				{Name: "ID", Kind: TypeInteger},
			},
		},
	}

	if arrayType.Kind != TypeArray {
		t.Errorf("expected array type, got %s", arrayType.Kind)
	}
	if arrayType.ElementType == nil {
		t.Fatal("expected ElementType to be set")
	}
	if arrayType.ElementType.Kind != TypeStruct {
		t.Errorf("expected element type struct, got %s", arrayType.ElementType.Kind)
	}
}

func TestDataType_JSONRoundTrip(t *testing.T) {
	original := &DataType{
		Name: "DECIMAL-TYPE",
		Kind: TypeDecimal,
		Size: 15,
		Scale: 2,
		Metadata: map[string]string{"cobol": "PIC 9(13)V99"},
	}

	data, err := json.Marshal(original)
	if err != nil {
		t.Fatalf("failed to marshal: %v", err)
	}

	var decoded DataType
	if err := json.Unmarshal(data, &decoded); err != nil {
		t.Fatalf("failed to unmarshal: %v", err)
	}

	if decoded.Name != "DECIMAL-TYPE" {
		t.Errorf("expected name DECIMAL-TYPE, got %s", decoded.Name)
	}
	if decoded.Kind != TypeDecimal {
		t.Errorf("expected kind decimal, got %s", decoded.Kind)
	}
	if decoded.Size != 15 {
		t.Errorf("expected size 15, got %d", decoded.Size)
	}
	if decoded.Scale != 2 {
		t.Errorf("expected scale 2, got %d", decoded.Scale)
	}
}

func TestBusinessRule_AllFields(t *testing.T) {
	rule := &BusinessRule{
		ID:          "BR-001",
		Description: "Customer balance must be non-negative",
		SourceRef:   "CUSTPROC.cbl:150-155",
		Confidence:  0.95,
		Tags:        []string{"validation", "business-logic"},
	}

	if rule.ID != "BR-001" {
		t.Errorf("expected ID BR-001, got %s", rule.ID)
	}
	if rule.Confidence != 0.95 {
		t.Errorf("expected confidence 0.95, got %f", rule.Confidence)
	}
	if len(rule.Tags) != 2 {
		t.Errorf("expected 2 tags, got %d", len(rule.Tags))
	}
}

func TestIOContract_AllIOKinds(t *testing.T) {
	tests := []struct {
		name string
		kind IOKind
		want string
	}{
		{"file IO", IOFile, "file"},
		{"database IO", IODB, "database"},
		{"screen IO", IOScreen, "screen"},
		{"queue IO", IOQueue, "queue"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			contract := &IOContract{
				Name: tt.name,
				Kind: tt.kind,
				Direction: IORead,
			}
			if string(contract.Kind) != tt.want {
				t.Errorf("expected kind %s, got %s", tt.want, contract.Kind)
			}
		})
	}
}

func TestIOContract_AllIODirections(t *testing.T) {
	tests := []struct {
		name      string
		direction IODirection
		want      string
	}{
		{"read only", IORead, "read"},
		{"write only", IOWrite, "write"},
		{"read-write", IOReadWrite, "readwrite"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			contract := &IOContract{
				Name:      tt.name,
				Kind:      IOFile,
				Direction: tt.direction,
			}
			if string(contract.Direction) != tt.want {
				t.Errorf("expected direction %s, got %s", tt.want, contract.Direction)
			}
		})
	}
}

func TestIOContract_WithSchema(t *testing.T) {
	contract := &IOContract{
		Name:      "CUSTOMER-FILE",
		Kind:      IOFile,
		Direction: IOReadWrite,
		Schema: &DataType{
			Name: "CUSTOMER-RECORD",
			Kind: TypeStruct,
			Fields: []*DataType{
				{Name: "CUST-ID", Kind: TypeInteger},
				{Name: "CUST-NAME", Kind: TypeString},
			},
		},
		Metadata: map[string]string{"format": "fixed-width"},
	}

	if contract.Schema == nil {
		t.Fatal("expected Schema to be set")
	}
	if len(contract.Schema.Fields) != 2 {
		t.Errorf("expected 2 fields, got %d", len(contract.Schema.Fields))
	}
}

func TestCallGraph_WithEdges(t *testing.T) {
	cg := &CallGraph{
		Edges: []CallEdge{
			{Caller: "MAIN-PROC", Callee: "VALIDATE-INPUT"},
			{Caller: "MAIN-PROC", Callee: "PROCESS-DATA"},
			{Caller: "PROCESS-DATA", Callee: "WRITE-OUTPUT"},
		},
	}

	if len(cg.Edges) != 3 {
		t.Errorf("expected 3 edges, got %d", len(cg.Edges))
	}
	if cg.Edges[0].Caller != "MAIN-PROC" {
		t.Errorf("expected caller MAIN-PROC, got %s", cg.Edges[0].Caller)
	}
	if cg.Edges[2].Callee != "WRITE-OUTPUT" {
		t.Errorf("expected callee WRITE-OUTPUT, got %s", cg.Edges[2].Callee)
	}
}

func TestCallGraph_JSONRoundTrip(t *testing.T) {
	original := &CallGraph{
		Edges: []CallEdge{
			{Caller: "A", Callee: "B"},
			{Caller: "B", Callee: "C"},
		},
	}

	data, err := json.Marshal(original)
	if err != nil {
		t.Fatalf("failed to marshal: %v", err)
	}

	var decoded CallGraph
	if err := json.Unmarshal(data, &decoded); err != nil {
		t.Fatalf("failed to unmarshal: %v", err)
	}

	if len(decoded.Edges) != 2 {
		t.Errorf("expected 2 edges, got %d", len(decoded.Edges))
	}
	if decoded.Edges[1].Caller != "B" || decoded.Edges[1].Callee != "C" {
		t.Errorf("edge mismatch: got %+v", decoded.Edges[1])
	}
}

func TestCFG_WithBasicBlocks(t *testing.T) {
	cfg := &CFG{
		Blocks: []*BasicBlock{
			{
				ID:         "bb1",
				Statements: []string{"MOVE 0 TO COUNTER"},
				Successors: []string{"bb2"},
			},
			{
				ID:         "bb2",
				Statements: []string{"ADD 1 TO COUNTER", "IF COUNTER > 10"},
				Successors: []string{"bb3", "bb4"},
			},
			{
				ID:         "bb3",
				Statements: []string{"DISPLAY 'DONE'"},
			},
		},
	}

	if len(cfg.Blocks) != 3 {
		t.Errorf("expected 3 blocks, got %d", len(cfg.Blocks))
	}
	if len(cfg.Blocks[1].Successors) != 2 {
		t.Errorf("expected 2 successors for bb2, got %d", len(cfg.Blocks[1].Successors))
	}
	if len(cfg.Blocks[2].Successors) != 0 {
		t.Errorf("expected 0 successors for bb3, got %d", len(cfg.Blocks[2].Successors))
	}
}

func TestBasicBlock_JSONRoundTrip(t *testing.T) {
	original := &BasicBlock{
		ID:         "entry",
		Statements: []string{"STMT1", "STMT2", "STMT3"},
		Successors: []string{"exit"},
	}

	data, err := json.Marshal(original)
	if err != nil {
		t.Fatalf("failed to marshal: %v", err)
	}

	var decoded BasicBlock
	if err := json.Unmarshal(data, &decoded); err != nil {
		t.Fatalf("failed to unmarshal: %v", err)
	}

	if decoded.ID != "entry" {
		t.Errorf("expected ID entry, got %s", decoded.ID)
	}
	if len(decoded.Statements) != 3 {
		t.Errorf("expected 3 statements, got %d", len(decoded.Statements))
	}
	if len(decoded.Successors) != 1 {
		t.Errorf("expected 1 successor, got %d", len(decoded.Successors))
	}
}

func TestComplexNestedStructure(t *testing.T) {
	// Test a complex nested structure with all types
	graph := &SemanticGraph{
		Modules: []*Module{
			{
				Name:     "CUSTPROC",
				Path:     "/src/CUSTPROC.cbl",
				Language: "COBOL",
				Functions: []*Function{
					{
						Name: "PROCESS-CUSTOMER",
						Parameters: []*Parameter{
							{
								Name: "customer-rec",
								Type: &DataType{
									Name: "CUSTOMER-RECORD",
									Kind: TypeStruct,
									Fields: []*DataType{
										{Name: "CUST-ID", Kind: TypeInteger, Size: 10},
										{Name: "CUST-NAME", Kind: TypeString, Size: 50},
									},
								},
								Optional: false,
							},
						},
						Returns: &DataType{Name: "STATUS-CODE", Kind: TypeInteger},
						CFG: &CFG{
							Blocks: []*BasicBlock{
								{ID: "entry", Statements: []string{"VALIDATE INPUT"}, Successors: []string{"process"}},
								{ID: "process", Statements: []string{"UPDATE RECORD"}, Successors: []string{"exit"}},
								{ID: "exit", Statements: []string{"RETURN 0"}},
							},
						},
						Calls: []string{"VALIDATE-CUSTOMER", "UPDATE-DB"},
					},
				},
				DataTypes: []*DataType{
					{
						Name: "CUSTOMER-ARRAY",
						Kind: TypeArray,
						ElementType: &DataType{
							Name: "CUSTOMER",
							Kind: TypeStruct,
						},
					},
				},
				IOContracts: []*IOContract{
					{
						Name:      "CUSTFILE",
						Kind:      IOFile,
						Direction: IOReadWrite,
						Schema: &DataType{Name: "CUST-REC", Kind: TypeStruct},
					},
				},
			},
		},
		CallGraph: &CallGraph{
			Edges: []CallEdge{
				{Caller: "PROCESS-CUSTOMER", Callee: "VALIDATE-CUSTOMER"},
			},
		},
		BusinessRules: []*BusinessRule{
			{
				ID:          "BR-001",
				Description: "Customer ID must be unique",
				SourceRef:   "CUSTPROC.cbl:100",
				Confidence:  0.95,
				Tags:        []string{"validation"},
			},
		},
	}

	// Marshal and unmarshal the complex structure
	data, err := json.Marshal(graph)
	if err != nil {
		t.Fatalf("failed to marshal complex graph: %v", err)
	}

	var decoded SemanticGraph
	if err := json.Unmarshal(data, &decoded); err != nil {
		t.Fatalf("failed to unmarshal complex graph: %v", err)
	}

	// Verify deep nested structures
	if len(decoded.Modules) != 1 {
		t.Errorf("expected 1 module, got %d", len(decoded.Modules))
	}

	mod := decoded.Modules[0]
	if len(mod.Functions) != 1 {
		t.Fatalf("expected 1 function, got %d", len(mod.Functions))
	}

	fn := mod.Functions[0]
	if len(fn.Parameters) != 1 {
		t.Errorf("expected 1 parameter, got %d", len(fn.Parameters))
	}
	if fn.Parameters[0].Type.Kind != TypeStruct {
		t.Errorf("expected parameter type struct, got %s", fn.Parameters[0].Type.Kind)
	}
	if len(fn.Parameters[0].Type.Fields) != 2 {
		t.Errorf("expected 2 fields in parameter type, got %d", len(fn.Parameters[0].Type.Fields))
	}

	if fn.CFG == nil {
		t.Fatal("expected CFG to be set")
	}
	if len(fn.CFG.Blocks) != 3 {
		t.Errorf("expected 3 CFG blocks, got %d", len(fn.CFG.Blocks))
	}

	if len(mod.DataTypes) != 1 {
		t.Errorf("expected 1 module data type, got %d", len(mod.DataTypes))
	}
	if mod.DataTypes[0].ElementType == nil {
		t.Error("expected array element type to be set")
	}

	if len(decoded.CallGraph.Edges) != 1 {
		t.Errorf("expected 1 call edge, got %d", len(decoded.CallGraph.Edges))
	}

	if len(decoded.BusinessRules) != 1 {
		t.Errorf("expected 1 business rule, got %d", len(decoded.BusinessRules))
	}
}
