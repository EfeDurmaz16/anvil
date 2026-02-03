package fortran

import (
	"context"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

func TestParseSimpleProgram(t *testing.T) {
	src := []byte(`
program hello
    implicit none
    print *, "Hello, World!"
end program hello
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "hello.f90", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(graph.Modules) != 1 {
		t.Fatalf("expected 1 module, got %d", len(graph.Modules))
	}
	if graph.Modules[0].Name != "HELLO" {
		t.Errorf("expected program name HELLO, got %s", graph.Modules[0].Name)
	}
	if len(graph.Modules[0].IOContracts) == 0 {
		t.Error("expected I/O contracts for print statement")
	}
}

func TestParseSubroutine(t *testing.T) {
	src := []byte(`
program main
    implicit none
    call greet("World")
end program main

subroutine greet(name)
    implicit none
    character(len=*), intent(in) :: name
    print *, "Hello, ", name
end subroutine greet
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "greet.f90", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}
	mod := graph.Modules[0]
	if len(mod.Functions) < 1 {
		t.Fatalf("expected at least 1 function, got %d", len(mod.Functions))
	}

	// Check for subroutine
	var greetFn *ir.Function
	for _, fn := range mod.Functions {
		if fn.Name == "GREET" {
			greetFn = fn
			break
		}
	}
	if greetFn == nil {
		t.Fatal("expected to find GREET subroutine")
	}
	if len(greetFn.Parameters) != 1 {
		t.Errorf("expected 1 parameter, got %d", len(greetFn.Parameters))
	}
	if greetFn.Parameters[0].Name != "NAME" {
		t.Errorf("expected parameter NAME, got %s", greetFn.Parameters[0].Name)
	}
	if greetFn.Parameters[0].Type.Kind != ir.TypeString {
		t.Errorf("expected string type, got %s", greetFn.Parameters[0].Type.Kind)
	}
	if greetFn.Parameters[0].Type.Metadata["intent"] != "in" {
		t.Errorf("expected intent(in), got %s", greetFn.Parameters[0].Type.Metadata["intent"])
	}
}

func TestParseFunction(t *testing.T) {
	src := []byte(`
program main
    implicit none
    real :: result
    result = add_numbers(5.0, 3.0)
    print *, result
end program main

function add_numbers(a, b) result(sum)
    implicit none
    real, intent(in) :: a, b
    real :: sum
    sum = a + b
end function add_numbers
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "calc.f90", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}

	mod := graph.Modules[0]
	var addFn *ir.Function
	for _, fn := range mod.Functions {
		if fn.Name == "ADD_NUMBERS" {
			addFn = fn
			break
		}
	}
	if addFn == nil {
		t.Fatal("expected to find ADD_NUMBERS function")
	}
	if len(addFn.Parameters) != 2 {
		t.Errorf("expected 2 parameters, got %d", len(addFn.Parameters))
	}
	if addFn.Returns == nil {
		t.Error("expected return type")
	}
}

func TestParseModule(t *testing.T) {
	src := []byte(`
module math_ops
    implicit none

contains

    function multiply(x, y)
        implicit none
        real, intent(in) :: x, y
        real :: multiply
        multiply = x * y
    end function multiply

    subroutine divide(x, y, result)
        implicit none
        real, intent(in) :: x, y
        real, intent(out) :: result
        if (y /= 0.0) then
            result = x / y
        end if
    end subroutine divide

end module math_ops
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "math_ops.f90", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}

	mod := graph.Modules[0]
	if mod.Name != "MATH_OPS" {
		t.Errorf("expected module name MATH_OPS, got %s", mod.Name)
	}
	if len(mod.Functions) < 2 {
		t.Fatalf("expected at least 2 functions, got %d", len(mod.Functions))
	}

	// Check multiply function
	var multiplyFn *ir.Function
	for _, fn := range mod.Functions {
		if fn.Name == "MULTIPLY" {
			multiplyFn = fn
			break
		}
	}
	if multiplyFn == nil {
		t.Fatal("expected to find MULTIPLY function")
	}

	// Check divide subroutine
	var divideFn *ir.Function
	for _, fn := range mod.Functions {
		if fn.Name == "DIVIDE" {
			divideFn = fn
			break
		}
	}
	if divideFn == nil {
		t.Fatal("expected to find DIVIDE subroutine")
	}
	if len(divideFn.Parameters) != 3 {
		t.Errorf("expected 3 parameters, got %d", len(divideFn.Parameters))
	}

	// Check intent(out)
	var resultParam *ir.Parameter
	for _, p := range divideFn.Parameters {
		if p.Name == "RESULT" {
			resultParam = p
			break
		}
	}
	if resultParam == nil {
		t.Fatal("expected to find RESULT parameter")
	}
	if resultParam.Type.Metadata["intent"] != "out" {
		t.Errorf("expected intent(out), got %s", resultParam.Type.Metadata["intent"])
	}
}

func TestParseVariableDeclarations(t *testing.T) {
	src := []byte(`
program vars
    implicit none
    integer :: count
    integer*8 :: big_number
    real :: temperature
    double precision :: precise_value
    complex :: wave
    logical :: flag
    character(len=50) :: name
    integer, dimension(10) :: array
    type(my_type) :: custom
end program vars
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "vars.f90", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}

	mod := graph.Modules[0]
	if len(mod.DataTypes) < 8 {
		t.Logf("Found data types:")
		for _, dt := range mod.DataTypes {
			t.Logf("  - %s (kind: %s)", dt.Name, dt.Kind)
		}
		t.Errorf("expected at least 8 data types, got %d", len(mod.DataTypes))
	}

	// Check specific types
	typeMap := make(map[string]*ir.DataType)
	for _, dt := range mod.DataTypes {
		typeMap[dt.Name] = dt
	}

	if dt, ok := typeMap["COUNT"]; ok {
		if dt.Kind != ir.TypeInteger {
			t.Errorf("expected integer type for COUNT, got %s", dt.Kind)
		}
	}

	if dt, ok := typeMap["BIG_NUMBER"]; ok {
		if dt.Kind != ir.TypeInteger {
			t.Errorf("expected integer type for BIG_NUMBER, got %s", dt.Kind)
		}
		if dt.Size != 8 {
			t.Errorf("expected size 8 for BIG_NUMBER, got %d", dt.Size)
		}
	}

	if dt, ok := typeMap["TEMPERATURE"]; ok {
		if dt.Kind != ir.TypeDecimal {
			t.Errorf("expected decimal type for TEMPERATURE, got %s", dt.Kind)
		}
	}

	if dt, ok := typeMap["PRECISE_VALUE"]; ok {
		if dt.Kind != ir.TypeDecimal {
			t.Errorf("expected decimal type for PRECISE_VALUE, got %s", dt.Kind)
		}
		if dt.Size != 8 {
			t.Errorf("expected size 8 for PRECISE_VALUE, got %d", dt.Size)
		}
	}

	if dt, ok := typeMap["FLAG"]; ok {
		if dt.Kind != ir.TypeBoolean {
			t.Errorf("expected boolean type for FLAG, got %s", dt.Kind)
		}
	}

	if dt, ok := typeMap["NAME"]; ok {
		if dt.Kind != ir.TypeString {
			t.Errorf("expected string type for NAME, got %s", dt.Kind)
		}
		if dt.Size != 50 {
			t.Errorf("expected size 50 for NAME, got %d", dt.Size)
		}
	}

	if dt, ok := typeMap["ARRAY"]; ok {
		if dt.Kind != ir.TypeArray {
			t.Errorf("expected array type for ARRAY, got %s", dt.Kind)
		}
		if dt.ElementType == nil {
			t.Error("expected element type for ARRAY")
		} else if dt.ElementType.Kind != ir.TypeInteger {
			t.Errorf("expected integer element type for ARRAY, got %s", dt.ElementType.Kind)
		}
	}

	if dt, ok := typeMap["CUSTOM"]; ok {
		if dt.Kind != ir.TypeStruct {
			t.Errorf("expected struct type for CUSTOM, got %s", dt.Kind)
		}
	}
}

func TestParseCallGraph(t *testing.T) {
	src := []byte(`
program main
    implicit none
    call process_data()
    call save_results()
end program main

subroutine process_data()
    implicit none
    call validate_input()
    call compute()
end subroutine process_data

subroutine validate_input()
    implicit none
    print *, "Validating..."
end subroutine validate_input

subroutine compute()
    implicit none
    print *, "Computing..."
end subroutine compute

subroutine save_results()
    implicit none
    print *, "Saving..."
end subroutine save_results
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "callgraph.f90", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}

	if len(graph.CallGraph.Edges) < 4 {
		t.Errorf("expected at least 4 call edges, got %d", len(graph.CallGraph.Edges))
	}

	// Check that process_data calls validate_input and compute
	processDataEdges := 0
	for _, edge := range graph.CallGraph.Edges {
		if edge.Caller == "PROCESS_DATA" && (edge.Callee == "VALIDATE_INPUT" || edge.Callee == "COMPUTE") {
			processDataEdges++
		}
	}
	if processDataEdges < 2 {
		t.Errorf("expected process_data to call validate_input and compute, got %d edges", processDataEdges)
	}
}

func TestParseIOStatements(t *testing.T) {
	src := []byte(`
program io_test
    implicit none
    integer :: unit_num
    real :: value

    ! Open file
    open(unit=10, file='data.txt', status='old')

    ! Read from file
    read(10, *) value

    ! Write to file
    write(10, *) value

    ! Print to screen
    print *, "Value: ", value

    close(10)
end program io_test
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "io.f90", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}

	mod := graph.Modules[0]
	if len(mod.IOContracts) < 4 {
		t.Errorf("expected at least 4 I/O contracts, got %d", len(mod.IOContracts))
	}

	// Check for different I/O types
	hasFileIO := false
	hasScreenIO := false
	for _, io := range mod.IOContracts {
		if io.Kind == ir.IOFile {
			hasFileIO = true
		}
		if io.Kind == ir.IOScreen {
			hasScreenIO = true
		}
	}
	if !hasFileIO {
		t.Error("expected file I/O contract")
	}
	if !hasScreenIO {
		t.Error("expected screen I/O contract")
	}
}

func TestParseUseStatement(t *testing.T) {
	src := []byte(`
program main
    use math_ops
    implicit none
    real :: x, y
    x = multiply(5.0, 3.0)
end program main
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "main.f90", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}

	mod := graph.Modules[0]
	if usedModules, ok := mod.Metadata["use_modules"]; !ok || usedModules == "" {
		t.Error("expected use_modules metadata")
	} else if usedModules != "math_ops" {
		t.Errorf("expected use_modules to contain 'math_ops', got %s", usedModules)
	}
}

func TestResolveDependencies(t *testing.T) {
	mathModule := []byte(`
module math_ops
    implicit none
    integer :: global_count
end module math_ops
`)
	mainProgram := []byte(`
program main
    use math_ops
    implicit none
    global_count = 10
end program main
`)

	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "math_ops.f90", Content: mathModule},
		{Path: "main.f90", Content: mainProgram},
	})
	if err != nil {
		t.Fatal(err)
	}

	err = p.ResolveDependencies(context.Background(), graph)
	if err != nil {
		t.Fatal(err)
	}

	// Check that main program has access to math_ops types
	var mainMod *ir.Module
	for _, mod := range graph.Modules {
		if mod.Name == "MAIN" {
			mainMod = mod
			break
		}
	}
	if mainMod == nil {
		t.Fatal("expected to find MAIN module")
	}

	// After dependency resolution, main should have reference to global_count
	hasGlobalCount := false
	for _, dt := range mainMod.DataTypes {
		if dt.Name == "GLOBAL_COUNT" && dt.Metadata["from_module"] == "MATH_OPS" {
			hasGlobalCount = true
			break
		}
	}
	if !hasGlobalCount {
		t.Error("expected main module to reference GLOBAL_COUNT from math_ops after dependency resolution")
	}
}

func TestParseFixedFormComments(t *testing.T) {
	src := []byte(`
C This is a comment in fixed form
      program test
* Another comment
      implicit none
! Free form comment
      print *, "Hello"
      end program test
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "comments.f", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}

	if len(graph.Modules) != 1 {
		t.Fatalf("expected 1 module, got %d", len(graph.Modules))
	}
	if graph.Modules[0].Name != "TEST" {
		t.Errorf("expected program name TEST, got %s", graph.Modules[0].Name)
	}
}
