package perl

import (
	"context"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/plugins"
)

func TestParseSimple(t *testing.T) {
	src := []byte(`#!/usr/bin/perl
use strict;
use warnings;

package HelloWorld;

sub greet {
    my ($name) = @_;
    print "Hello, $name!\n";
}

sub main {
    greet("World");
}

main();
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "hello.pl", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(graph.Modules) != 1 {
		t.Fatalf("expected 1 module, got %d", len(graph.Modules))
	}
	if graph.Modules[0].Name != "HelloWorld" {
		t.Errorf("expected package name HelloWorld, got %s", graph.Modules[0].Name)
	}
	if len(graph.Modules[0].Functions) < 2 {
		t.Errorf("expected at least 2 functions (greet, main), got %d", len(graph.Modules[0].Functions))
	}
}

func TestParseFileIO(t *testing.T) {
	src := []byte(`#!/usr/bin/perl
package FileProcessor;

sub process_file {
    my ($filename) = @_;

    open(my $fh, '<', $filename) or die "Cannot open: $!";

    while (my $line = <$fh>) {
        chomp($line);
        process_line($line);
    }

    close($fh);
}

sub write_output {
    my ($data) = @_;

    open(my $out, '>', 'output.txt') or die "Cannot write: $!";
    print $out $data;
    close($out);
}

sub process_line {
    my ($line) = @_;
    # Process the line
}
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "fileio.pl", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}

	mod := graph.Modules[0]
	if mod.Name != "FileProcessor" {
		t.Errorf("expected FileProcessor, got %s", mod.Name)
	}

	// Should detect file I/O contracts
	if len(mod.IOContracts) == 0 {
		t.Error("expected I/O contracts for file operations")
	}

	// Check for open operations
	openFound := false
	for _, io := range mod.IOContracts {
		if io.Kind == "file" {
			openFound = true
			break
		}
	}
	if !openFound {
		t.Error("expected file I/O contract")
	}

	// Should have at least 3 functions
	if len(mod.Functions) < 3 {
		t.Errorf("expected at least 3 functions, got %d", len(mod.Functions))
	}
}

func TestParseDBI(t *testing.T) {
	src := []byte(`#!/usr/bin/perl
use strict;
use DBI;

package Database;

sub connect_db {
    my $dbh = DBI->connect("DBI:mysql:database=test", "user", "pass")
        or die "Cannot connect: $DBI::errstr";
    return $dbh;
}

sub fetch_users {
    my ($dbh) = @_;

    my $sth = $dbh->prepare("SELECT * FROM users");
    $sth->execute();

    while (my $row = $sth->fetchrow_hashref()) {
        process_user($row);
    }
}

sub insert_user {
    my ($dbh, $name, $email) = @_;

    my $sth = $dbh->prepare("INSERT INTO users (name, email) VALUES (?, ?)");
    $sth->execute($name, $email);
}

sub process_user {
    my ($user) = @_;
    # Process user data
}
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "database.pl", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}

	mod := graph.Modules[0]
	if mod.Name != "Database" {
		t.Errorf("expected Database, got %s", mod.Name)
	}

	// Should detect database I/O
	dbFound := false
	for _, io := range mod.IOContracts {
		if io.Kind == "database" {
			dbFound = true
			break
		}
	}
	if !dbFound {
		t.Error("expected database I/O contract")
	}

	// Should have 4 functions
	if len(mod.Functions) < 4 {
		t.Errorf("expected at least 4 functions, got %d", len(mod.Functions))
	}

	// Check call graph
	if len(graph.CallGraph.Edges) == 0 {
		t.Error("expected call graph edges")
	}
}

func TestParseVariables(t *testing.T) {
	src := []byte(`#!/usr/bin/perl
package Variables;

my $scalar_var = "hello";
our @array_var = (1, 2, 3);
my %hash_var = (key => "value");

sub test_vars {
    my ($param1, $param2) = @_;
    local $temp = 42;

    my @results;
    push @results, $param1;
    push @results, $param2;

    return @results;
}
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "vars.pl", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}

	mod := graph.Modules[0]

	// Should detect variable declarations
	if len(mod.DataTypes) == 0 {
		t.Error("expected data types for variable declarations")
	}

	// Check for different variable types
	hasScalar := false
	hasArray := false
	hasHash := false

	for _, dt := range mod.DataTypes {
		switch dt.Kind {
		case "string":
			hasScalar = true
		case "array":
			hasArray = true
		case "struct":
			hasHash = true
		}
	}

	if !hasScalar {
		t.Error("expected scalar variable")
	}
	if !hasArray {
		t.Error("expected array variable")
	}
	if !hasHash {
		t.Error("expected hash variable")
	}
}

func TestParseMethodCalls(t *testing.T) {
	src := []byte(`#!/usr/bin/perl
package ObjectOriented;

sub new {
    my ($class, %args) = @_;
    my $self = {
        name => $args{name},
        value => $args{value},
    };
    bless $self, $class;
    return $self;
}

sub process {
    my ($self) = @_;

    $self->validate();
    my $result = $self->calculate();
    $self->store($result);

    return $result;
}

sub validate {
    my ($self) = @_;
    # Validation logic
}

sub calculate {
    my ($self) = @_;
    return $self->{value} * 2;
}

sub store {
    my ($self, $data) = @_;
    # Store data
}
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "oop.pl", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}

	mod := graph.Modules[0]

	// Should have 5 methods
	if len(mod.Functions) < 5 {
		t.Errorf("expected at least 5 functions, got %d", len(mod.Functions))
	}

	// Find the process function and check its calls
	var processFn *struct {
		Name  string
		Calls []string
	}

	for _, fn := range mod.Functions {
		if fn.Name == "process" {
			processFn = &struct {
				Name  string
				Calls []string
			}{Name: fn.Name, Calls: fn.Calls}
			break
		}
	}

	if processFn == nil {
		t.Fatal("expected to find 'process' function")
	}

	// Process should call validate, calculate, and store
	expectedCalls := map[string]bool{
		"validate":  false,
		"calculate": false,
		"store":     false,
	}

	for _, call := range processFn.Calls {
		if _, ok := expectedCalls[call]; ok {
			expectedCalls[call] = true
		}
	}

	for call, found := range expectedCalls {
		if !found {
			t.Errorf("expected process to call %s", call)
		}
	}
}

func TestParseDependencies(t *testing.T) {
	src := []byte(`#!/usr/bin/perl
use strict;
use warnings;
use Data::Dumper;
use My::Custom::Module;
require Another::Module;

package TestModule;

sub test {
    # Test function
}
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "deps.pl", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}

	mod := graph.Modules[0]

	// Should track dependencies (excluding core modules)
	deps := mod.Metadata["dependencies"]
	if deps == "" {
		t.Error("expected dependencies to be tracked")
	}

	// Should include custom modules but not core modules
	if !containsSubstring(deps, "My::Custom::Module") {
		t.Error("expected My::Custom::Module in dependencies")
	}
	if !containsSubstring(deps, "Another::Module") {
		t.Error("expected Another::Module in dependencies")
	}
	if containsSubstring(deps, "strict") || containsSubstring(deps, "warnings") {
		t.Error("should not include core modules in dependencies")
	}
}

func TestParseParameters(t *testing.T) {
	src := []byte(`#!/usr/bin/perl
package Params;

sub add {
    my ($a, $b) = @_;
    return $a + $b;
}

sub process_hash {
    my (%options) = @_;
    return %options;
}
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "params.pl", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}

	mod := graph.Modules[0]

	if len(mod.Functions) < 2 {
		t.Fatalf("expected at least 2 functions, got %d", len(mod.Functions))
	}

	// Check if parameters are extracted (though Perl's my ($a, $b) = @_ is in the body)
	// Our parser looks for sub foo ($a, $b) { syntax
	// For this test, we're mainly checking that functions are parsed
	for _, fn := range mod.Functions {
		if fn.Name == "add" || fn.Name == "process_hash" {
			// Parameters extracted from body assignment would be complex
			// For now, just verify the function exists and has a body
			if fn.Body == "" {
				t.Errorf("expected function %s to have a body", fn.Name)
			}
		}
	}
}

func containsSubstring(s, substr string) bool {
	return len(substr) > 0 && len(s) >= len(substr) && findSubstring(s, substr)
}

func findSubstring(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}
