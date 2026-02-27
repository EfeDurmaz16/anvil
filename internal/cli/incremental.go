package cli

import (
	"fmt"
	"log/slog"
	"os"
	"path/filepath"

	"github.com/efebarandurmaz/anvil/internal/migration"
	"github.com/efebarandurmaz/anvil/internal/plugins"
	"github.com/spf13/cobra"
)

// ReadSourceFiles reads source files from a directory based on language.
func ReadSourceFiles(dir, lang string) ([]plugins.SourceFile, error) {
	var patterns []string
	switch lang {
	case "cobol":
		patterns = []string{"*.cbl", "*.cob", "*.cpy", "*.CBL", "*.COB", "*.CPY"}
	case "perl":
		patterns = []string{"*.pl", "*.pm"}
	case "fortran":
		patterns = []string{"*.f", "*.f90", "*.for", "*.F", "*.F90"}
	default:
		patterns = []string{"*"}
	}

	var files []plugins.SourceFile
	err := filepath.WalkDir(dir, func(path string, d os.DirEntry, walkErr error) error {
		if walkErr != nil || d.IsDir() {
			return walkErr
		}
		for _, pat := range patterns {
			if matched, _ := filepath.Match(pat, d.Name()); matched {
				content, readErr := os.ReadFile(path)
				if readErr != nil {
					return readErr
				}
				relPath, _ := filepath.Rel(dir, path)
				files = append(files, plugins.SourceFile{Path: relPath, Content: content})
				break
			}
		}
		return nil
	})
	return files, err
}

// RunIncrementalAnalyze analyzes source directory for incremental migration.
func RunIncrementalAnalyze(sourceDir, outputDir, sourceLang, targetLang string, force, dryRun bool) error {
	slog.Info("incremental analyze", "source_dir", sourceDir, "output_dir", outputDir)

	// Read source files
	files, err := ReadSourceFiles(sourceDir, sourceLang)
	if err != nil {
		return fmt.Errorf("read source files: %w", err)
	}

	slog.Info("read source files", "count", len(files))

	// Create incremental config
	cfg := &migration.IncrementalConfig{
		OutputDir:      outputDir,
		SourceLanguage: sourceLang,
		TargetLanguage: targetLang,
		Dependencies:   make(map[string][]string), // No dependency analysis for now
		ForceAll:       force,
		DryRun:         dryRun,
	}

	// Create runner and analyze
	runner := migration.NewIncrementalRunner(cfg)
	result, err := runner.Analyze(files)
	if err != nil {
		return fmt.Errorf("analyze: %w", err)
	}

	// Print report
	fmt.Print(migration.FormatIncrementalReport(result))

	// Save state unless dry-run
	if !dryRun {
		if err := os.MkdirAll(outputDir, 0o755); err != nil {
			return fmt.Errorf("create output dir: %w", err)
		}
		if err := runner.SaveState(files); err != nil {
			return fmt.Errorf("save state: %w", err)
		}
		slog.Info("state saved", "path", filepath.Join(outputDir, ".anvil-state.json"))
	}

	return nil
}

// RunIncrementalStatus shows current incremental migration status.
func RunIncrementalStatus(outputDir string) error {
	// Load state
	state, err := migration.LoadState(outputDir)
	if err != nil {
		return fmt.Errorf("load state: %w", err)
	}

	if state == nil {
		fmt.Println("No incremental state found. Run 'anvil incremental analyze' first.")
		return nil
	}

	// Load cache
	cache, err := migration.LoadCache(outputDir)
	if err != nil {
		return fmt.Errorf("load cache: %w", err)
	}

	// Print state info
	fmt.Println("+===========================================+")
	fmt.Println("|     Incremental Migration Status          |")
	fmt.Println("+===========================================+")
	fmt.Printf("| Last Run:        %s\n", state.LastRun.Format("2006-01-02 15:04:05"))
	fmt.Printf("| Source Language: %s\n", state.SourceLanguage)
	fmt.Printf("| Target Language: %s\n", state.TargetLanguage)
	fmt.Printf("| Fingerprints:    %d files\n", len(state.Fingerprints))
	fmt.Println("+===========================================+")

	// Print cache stats
	stats := cache.Stats()
	fmt.Printf("| Cache Entries:   %d\n", stats.TotalEntries)
	fmt.Printf("|   Success:       %d\n", stats.SuccessEntries)
	fmt.Printf("|   Failed:        %d\n", stats.FailedEntries)
	fmt.Printf("| Total Tokens:    %d\n", stats.TotalTokens)
	fmt.Println("+===========================================+")

	return nil
}

// RunIncrementalReset resets incremental state.
func RunIncrementalReset(outputDir string) error {
	stateFile := filepath.Join(outputDir, ".anvil-state.json")
	cacheFile := filepath.Join(outputDir, ".anvil-cache.json")

	var deleted []string

	// Delete state file
	if err := os.Remove(stateFile); err != nil {
		if !os.IsNotExist(err) {
			return fmt.Errorf("delete state file: %w", err)
		}
	} else {
		deleted = append(deleted, stateFile)
	}

	// Delete cache file
	if err := os.Remove(cacheFile); err != nil {
		if !os.IsNotExist(err) {
			return fmt.Errorf("delete cache file: %w", err)
		}
	} else {
		deleted = append(deleted, cacheFile)
	}

	if len(deleted) == 0 {
		fmt.Println("No incremental state found to reset.")
	} else {
		fmt.Println("Reset complete. Deleted files:")
		for _, f := range deleted {
			fmt.Printf("  - %s\n", f)
		}
	}

	return nil
}

// RunIncrementalDemo runs a demo showing incremental analysis.
func RunIncrementalDemo(format string) error {
	// Create temp dir
	tmpDir, err := os.MkdirTemp("", "anvil-incremental-demo-")
	if err != nil {
		return fmt.Errorf("create temp dir: %w", err)
	}
	defer os.RemoveAll(tmpDir)

	sourceDir := filepath.Join(tmpDir, "source")
	outputDir := filepath.Join(tmpDir, "output")

	if err := os.MkdirAll(sourceDir, 0o755); err != nil {
		return err
	}
	if err := os.MkdirAll(outputDir, 0o755); err != nil {
		return err
	}

	fmt.Println("+===========================================+")
	fmt.Println("|  Incremental Migration Demo               |")
	fmt.Println("+===========================================+")
	fmt.Println()

	// Create initial COBOL files
	files := map[string]string{
		"account.cbl": `       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCOUNT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ACCOUNT-BALANCE PIC 9(10)V99.
       PROCEDURE DIVISION.
           DISPLAY "Account Balance: " ACCOUNT-BALANCE.
           STOP RUN.`,
		"payment.cbl": `       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYMENT.
       PROCEDURE DIVISION.
           DISPLAY "Processing payment".
           STOP RUN.`,
		"ledger.cbl": `       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEDGER.
       PROCEDURE DIVISION.
           DISPLAY "Ledger system".
           STOP RUN.`,
	}

	for name, content := range files {
		if err := os.WriteFile(filepath.Join(sourceDir, name), []byte(content), 0o644); err != nil {
			return err
		}
	}

	fmt.Println("Step 1: First run (all files are new)")
	fmt.Println("---------------------------------------------")

	// First analysis
	if err := RunIncrementalAnalyze(sourceDir, outputDir, "cobol", "typescript", false, false); err != nil {
		return err
	}

	fmt.Println()
	fmt.Println("Step 2: Modify one file, add a new one")
	fmt.Println("---------------------------------------------")

	// Modify payment.cbl
	modifiedContent := `       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYMENT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PAYMENT-AMOUNT PIC 9(10)V99.
       PROCEDURE DIVISION.
           DISPLAY "Processing payment: " PAYMENT-AMOUNT.
           STOP RUN.`
	if err := os.WriteFile(filepath.Join(sourceDir, "payment.cbl"), []byte(modifiedContent), 0o644); err != nil {
		return err
	}

	// Add new file
	newFile := `       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORT.
       PROCEDURE DIVISION.
           DISPLAY "Generating report".
           STOP RUN.`
	if err := os.WriteFile(filepath.Join(sourceDir, "report.cbl"), []byte(newFile), 0o644); err != nil {
		return err
	}

	fmt.Println()

	// Second analysis
	if err := RunIncrementalAnalyze(sourceDir, outputDir, "cobol", "typescript", false, false); err != nil {
		return err
	}

	fmt.Println()
	fmt.Println("Demo complete!")
	fmt.Printf("Temp directory: %s\n", tmpDir)

	return nil
}

// RegisterIncrementalCommands registers all incremental subcommands.
func RegisterIncrementalCommands(parent *cobra.Command) {
	incrementalCmd := &cobra.Command{
		Use:   "incremental",
		Short: "Incremental migration operations",
	}

	var (
		incAnalyzeOutputDir  string
		incAnalyzeSourceLang string
		incAnalyzeTargetLang string
		incAnalyzeForce      bool
		incAnalyzeDryRun     bool
	)

	incrementalAnalyzeCmd := &cobra.Command{
		Use:   "analyze <source-dir>",
		Short: "Analyze a source directory for incremental migration",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return RunIncrementalAnalyze(args[0], incAnalyzeOutputDir, incAnalyzeSourceLang, incAnalyzeTargetLang, incAnalyzeForce, incAnalyzeDryRun)
		},
	}
	incrementalAnalyzeCmd.Flags().StringVar(&incAnalyzeOutputDir, "output-dir", ".anvil-output", "Output directory for state and cache")
	incrementalAnalyzeCmd.Flags().StringVar(&incAnalyzeSourceLang, "source-lang", "cobol", "Source language")
	incrementalAnalyzeCmd.Flags().StringVar(&incAnalyzeTargetLang, "target-lang", "typescript", "Target language")
	incrementalAnalyzeCmd.Flags().BoolVar(&incAnalyzeForce, "force", false, "Force full re-migration")
	incrementalAnalyzeCmd.Flags().BoolVar(&incAnalyzeDryRun, "dry-run", false, "Only report, don't save state")

	incrementalStatusCmd := &cobra.Command{
		Use:   "status <output-dir>",
		Short: "Show current incremental migration status",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return RunIncrementalStatus(args[0])
		},
	}

	incrementalResetCmd := &cobra.Command{
		Use:   "reset <output-dir>",
		Short: "Reset incremental state (delete state + cache files)",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return RunIncrementalReset(args[0])
		},
	}

	var incDemoFormat string
	incrementalDemoCmd := &cobra.Command{
		Use:   "demo",
		Short: "Run a demo showing incremental analysis",
		RunE: func(cmd *cobra.Command, args []string) error {
			return RunIncrementalDemo(incDemoFormat)
		},
	}
	incrementalDemoCmd.Flags().StringVar(&incDemoFormat, "format", "text", "Output format (text or json)")

	incrementalCmd.AddCommand(incrementalAnalyzeCmd, incrementalStatusCmd, incrementalResetCmd, incrementalDemoCmd)

	parent.AddCommand(incrementalCmd)
}
