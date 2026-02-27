package cli

import (
	"encoding/json"
	"fmt"
	"log/slog"
	"os"

	"github.com/efebarandurmaz/anvil/internal/depgraph"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/spf13/cobra"
)

// RunDepgraphAnalyze analyzes dependencies from a SemanticGraph JSON file.
func RunDepgraphAnalyze(irFile, format, output string) error {
	// Read the IR file
	data, err := os.ReadFile(irFile)
	if err != nil {
		return fmt.Errorf("read IR file: %w", err)
	}

	// Unmarshal to SemanticGraph
	var sg ir.SemanticGraph
	if err := json.Unmarshal(data, &sg); err != nil {
		return fmt.Errorf("unmarshal SemanticGraph: %w", err)
	}

	// Analyze
	graph := depgraph.Analyze(&sg)

	// Export in requested format
	result, err := ExportDepgraph(graph, format)
	if err != nil {
		return err
	}

	// Write to output
	if output == "" {
		fmt.Print(result)
	} else {
		if err := os.WriteFile(output, []byte(result), 0644); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
		slog.Info("wrote output", "path", output)
	}

	return nil
}

// RunDepgraphDemo generates a demo dependency graph.
func RunDepgraphDemo(format, output string) error {
	// Build demo SemanticGraph
	sg := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "accounts",
				Language: "cobol",
				Functions: []*ir.Function{
					{
						Name:  "ProcessPayment",
						Calls: []string{"ValidateAccount", "LogTransaction"},
					},
					{
						Name: "ValidateAccount",
					},
					{
						Name: "GetBalance",
					},
				},
				DataTypes: []*ir.DataType{
					{Name: "Account", Kind: ir.TypeStruct},
					{Name: "Payment", Kind: ir.TypeStruct},
				},
				IOContracts: []*ir.IOContract{
					{
						Name:      "AccountDB",
						Kind:      ir.IODB,
						Direction: ir.IOReadWrite,
					},
				},
			},
			{
				Name:     "ledger",
				Language: "cobol",
				Functions: []*ir.Function{
					{
						Name:  "LogTransaction",
						Calls: []string{"UpdateBalance"},
					},
					{
						Name: "UpdateBalance",
					},
					{
						Name:  "GenerateReport",
						Calls: []string{"GetBalance"},
					},
				},
				DataTypes: []*ir.DataType{
					{Name: "Transaction", Kind: ir.TypeStruct},
					{Name: "LedgerEntry", Kind: ir.TypeStruct},
				},
				IOContracts: []*ir.IOContract{
					{
						Name:      "TransactionLog",
						Kind:      ir.IOFile,
						Direction: ir.IOWrite,
					},
				},
			},
			{
				Name:     "reporting",
				Language: "cobol",
				Functions: []*ir.Function{
					{
						Name:  "DailyReport",
						Calls: []string{"GenerateReport", "GetBalance"},
					},
					{
						Name: "ExportCSV",
					},
				},
				DataTypes: []*ir.DataType{
					{Name: "Report", Kind: ir.TypeStruct},
				},
				IOContracts: []*ir.IOContract{
					{
						Name:      "ReportOutput",
						Kind:      ir.IOFile,
						Direction: ir.IOWrite,
					},
				},
			},
		},
	}

	// Analyze
	graph := depgraph.Analyze(sg)

	// Export in requested format
	result, err := ExportDepgraph(graph, format)
	if err != nil {
		return err
	}

	// Write to output
	if output == "" {
		fmt.Print(result)
	} else {
		if err := os.WriteFile(output, []byte(result), 0644); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
		slog.Info("wrote output", "path", output)
	}

	return nil
}

// ExportDepgraph exports a graph in the requested format.
func ExportDepgraph(graph *depgraph.Graph, format string) (string, error) {
	switch format {
	case "dot":
		return depgraph.ExportDOT(graph), nil
	case "mermaid":
		return depgraph.ExportMermaid(graph), nil
	case "json":
		data, err := depgraph.ExportJSON(graph)
		if err != nil {
			return "", fmt.Errorf("export JSON: %w", err)
		}
		return string(data), nil
	case "stats":
		return depgraph.FormatStats(graph), nil
	default:
		return "", fmt.Errorf("unsupported format: %s (supported: dot, mermaid, json, stats)", format)
	}
}

// RegisterDepgraphCommands registers all depgraph subcommands.
func RegisterDepgraphCommands(parent *cobra.Command) {
	depgraphCmd := &cobra.Command{
		Use:   "depgraph",
		Short: "Cross-module dependency graph analysis",
	}

	var (
		depgraphFormat string
		depgraphOutput string
	)

	analyzeCmd := &cobra.Command{
		Use:   "analyze <ir-file>",
		Short: "Analyze dependencies from a SemanticGraph JSON file",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return RunDepgraphAnalyze(args[0], depgraphFormat, depgraphOutput)
		},
	}
	analyzeCmd.Flags().StringVar(&depgraphFormat, "format", "stats", "Output format (dot/mermaid/json/stats)")
	analyzeCmd.Flags().StringVarP(&depgraphOutput, "output", "o", "", "Output file (default: stdout)")

	demoCmd := &cobra.Command{
		Use:   "demo",
		Short: "Generate a demo dependency graph",
		RunE: func(cmd *cobra.Command, args []string) error {
			return RunDepgraphDemo(depgraphFormat, depgraphOutput)
		},
	}
	demoCmd.Flags().StringVar(&depgraphFormat, "format", "stats", "Output format (dot/mermaid/json/stats)")
	demoCmd.Flags().StringVarP(&depgraphOutput, "output", "o", "", "Output file (default: stdout)")

	depgraphCmd.AddCommand(analyzeCmd, demoCmd)

	parent.AddCommand(depgraphCmd)
}
