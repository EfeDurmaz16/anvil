package cli

import (
	"context"
	"fmt"
	"log/slog"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/efebarandurmaz/anvil/internal/dashboard"
	"github.com/efebarandurmaz/anvil/internal/proofexplorer"
	"github.com/spf13/cobra"
)

// RunDashboard starts the dashboard server with optional demo data.
func RunDashboard(port int, demo bool) error {
	ctx := context.Background()

	// Create store and hub
	store := dashboard.NewStore()
	hub := dashboard.NewHub()

	// Seed demo data if requested
	if demo {
		SeedDemoData(store)
		slog.Info("Seeded demo migration data")
	}

	// Create and start server
	config := &dashboard.Config{
		ListenAddr: fmt.Sprintf(":%d", port),
	}
	server := dashboard.NewServer(config, store, hub)

	// Handle graceful shutdown
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, os.Interrupt, syscall.SIGTERM)

	errChan := make(chan error, 1)
	go func() {
		errChan <- server.Start()
	}()

	fmt.Printf("\nAnvil Dashboard\n")
	fmt.Printf("  Server running at: http://localhost:%d\n", port)
	if demo {
		fmt.Printf("  Demo data: 5 sample migrations loaded\n")
	}
	fmt.Printf("\nPress Ctrl+C to stop.\n\n")

	select {
	case err := <-errChan:
		return err
	case <-sigChan:
		slog.Info("Shutdown signal received")
		shutdownCtx, cancel := context.WithTimeout(ctx, 5*time.Second)
		defer cancel()
		return server.Stop(shutdownCtx)
	}
}

// RunExplore starts the proof pack explorer server.
func RunExplore(port int, dirs []string, open bool) error {
	ctx := context.Background()

	// Create explorer config
	config := &proofexplorer.Config{
		ListenAddr: fmt.Sprintf(":%d", port),
		PackDirs:   dirs,
	}

	// Create explorer
	explorer, err := proofexplorer.New(config)
	if err != nil {
		return fmt.Errorf("create explorer: %w", err)
	}

	// Handle graceful shutdown
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, os.Interrupt, syscall.SIGTERM)

	errChan := make(chan error, 1)
	go func() {
		errChan <- explorer.Start()
	}()

	fmt.Printf("\nAnvil Proof Pack Explorer\n")
	fmt.Printf("  Server running at: http://localhost:%d\n", port)
	fmt.Printf("  Scanning directories: %v\n", dirs)
	fmt.Printf("  Found %d proof packs\n", explorer.PackCount())
	if open {
		fmt.Printf("\n  Open in browser: http://localhost:%d\n", port)
	}
	fmt.Printf("\nPress Ctrl+C to stop.\n\n")

	select {
	case err := <-errChan:
		return err
	case <-sigChan:
		slog.Info("Shutdown signal received")
		shutdownCtx, cancel := context.WithTimeout(ctx, 5*time.Second)
		defer cancel()
		return explorer.Stop(shutdownCtx)
	}
}

// SeedDemoData creates sample migration runs for testing the dashboard.
func SeedDemoData(store *dashboard.Store) {
	now := time.Now()

	// Migration 1: Completed COBOL -> Java
	completed1 := now.Add(-2 * time.Hour)
	run1 := &dashboard.MigrationRun{
		ID:           "demo-1",
		Name:         "Legacy Payroll System",
		SourceLang:   "cobol",
		TargetLang:   "java",
		Status:       dashboard.StatusCompleted,
		StartedAt:    now.Add(-3 * time.Hour),
		CompletedAt:  &completed1,
		InputModules: 42,
		OutputFiles:  38,
		TestsPassed:  35,
		TestsFailed:  3,
		LLMCalls:     156,
		TotalTokens:  245000,
		Stages: []dashboard.StageResult{
			{
				Stage:     dashboard.StageCartographer,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-3 * time.Hour),
				Metrics:   dashboard.StageMetrics{InputItems: 42, OutputItems: 42},
			},
			{
				Stage:     dashboard.StageSpecular,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-3*time.Hour + 5*time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 42, OutputItems: 127, LLMCalls: 42, Tokens: 89000},
			},
			{
				Stage:     dashboard.StageArchitect,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-3*time.Hour + 12*time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 42, OutputItems: 38, LLMCalls: 84, Tokens: 134000},
			},
			{
				Stage:     dashboard.StageJudge,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-3*time.Hour + 28*time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 38, OutputItems: 38, LLMCalls: 30, Tokens: 22000},
			},
		},
	}
	store.CreateRun(run1)

	// Migration 2: Running Fortran -> Python
	run2 := &dashboard.MigrationRun{
		ID:           "demo-2",
		Name:         "Scientific Computing Library",
		SourceLang:   "fortran",
		TargetLang:   "python",
		Status:       dashboard.StatusRunning,
		StartedAt:    now.Add(-15 * time.Minute),
		InputModules: 28,
		LLMCalls:     64,
		TotalTokens:  98000,
		Stages: []dashboard.StageResult{
			{
				Stage:     dashboard.StageCartographer,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-15 * time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 28, OutputItems: 28},
			},
			{
				Stage:     dashboard.StageSpecular,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-12 * time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 28, OutputItems: 84, LLMCalls: 28, Tokens: 45000},
			},
			{
				Stage:     dashboard.StageArchitect,
				Status:    dashboard.StatusRunning,
				StartedAt: now.Add(-8 * time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 28, LLMCalls: 36, Tokens: 53000},
			},
		},
	}
	store.CreateRun(run2)

	// Migration 3: Failed COBOL -> TypeScript
	run3 := &dashboard.MigrationRun{
		ID:           "demo-3",
		Name:         "Mainframe Banking App",
		SourceLang:   "cobol",
		TargetLang:   "typescript",
		Status:       dashboard.StatusFailed,
		StartedAt:    now.Add(-5 * time.Hour),
		CompletedAt:  func() *time.Time { t := now.Add(-4*time.Hour + 30*time.Minute); return &t }(),
		Error:        "Judge validation failed: critical business logic mismatch in transaction processing",
		InputModules: 67,
		LLMCalls:     203,
		TotalTokens:  387000,
		Stages: []dashboard.StageResult{
			{
				Stage:     dashboard.StageCartographer,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-5 * time.Hour),
				Metrics:   dashboard.StageMetrics{InputItems: 67, OutputItems: 67},
			},
			{
				Stage:     dashboard.StageSpecular,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-5*time.Hour + 8*time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 67, OutputItems: 198, LLMCalls: 67, Tokens: 134000},
			},
			{
				Stage:     dashboard.StageArchitect,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-5*time.Hour + 18*time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 67, OutputItems: 54, LLMCalls: 95, Tokens: 189000},
			},
			{
				Stage:     dashboard.StageJudge,
				Status:    dashboard.StatusFailed,
				StartedAt: now.Add(-5*time.Hour + 42*time.Minute),
				Error:     "Validation failed: business logic mismatch",
				Metrics:   dashboard.StageMetrics{InputItems: 54, LLMCalls: 41, Tokens: 64000},
			},
		},
	}
	store.CreateRun(run3)

	// Migration 4: Completed Perl -> Go
	completed4 := now.Add(-1 * time.Hour)
	run4 := &dashboard.MigrationRun{
		ID:           "demo-4",
		Name:         "Legacy Web Services",
		SourceLang:   "perl",
		TargetLang:   "go",
		Status:       dashboard.StatusCompleted,
		StartedAt:    now.Add(-90 * time.Minute),
		CompletedAt:  &completed4,
		InputModules: 15,
		OutputFiles:  12,
		TestsPassed:  12,
		TestsFailed:  0,
		LLMCalls:     58,
		TotalTokens:  87000,
		Stages: []dashboard.StageResult{
			{
				Stage:     dashboard.StageCartographer,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-90 * time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 15, OutputItems: 15},
			},
			{
				Stage:     dashboard.StageSpecular,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-85 * time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 15, OutputItems: 42, LLMCalls: 15, Tokens: 28000},
			},
			{
				Stage:     dashboard.StageArchitect,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-78 * time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 15, OutputItems: 12, LLMCalls: 32, Tokens: 47000},
			},
			{
				Stage:     dashboard.StageJudge,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-68 * time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 12, OutputItems: 12, LLMCalls: 11, Tokens: 12000},
			},
		},
	}
	store.CreateRun(run4)

	// Migration 5: Completed COBOL -> Java (older)
	completed5 := now.Add(-24 * time.Hour)
	run5 := &dashboard.MigrationRun{
		ID:           "demo-5",
		Name:         "Inventory Management System",
		SourceLang:   "cobol",
		TargetLang:   "java",
		Status:       dashboard.StatusCompleted,
		StartedAt:    now.Add(-25 * time.Hour),
		CompletedAt:  &completed5,
		InputModules: 33,
		OutputFiles:  29,
		TestsPassed:  27,
		TestsFailed:  2,
		LLMCalls:     124,
		TotalTokens:  198000,
		Stages: []dashboard.StageResult{
			{
				Stage:     dashboard.StageCartographer,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-25 * time.Hour),
				Metrics:   dashboard.StageMetrics{InputItems: 33, OutputItems: 33},
			},
			{
				Stage:     dashboard.StageSpecular,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-25*time.Hour + 4*time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 33, OutputItems: 98, LLMCalls: 33, Tokens: 67000},
			},
			{
				Stage:     dashboard.StageArchitect,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-25*time.Hour + 15*time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 33, OutputItems: 29, LLMCalls: 67, Tokens: 109000},
			},
			{
				Stage:     dashboard.StageJudge,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-25*time.Hour + 38*time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 29, OutputItems: 29, LLMCalls: 24, Tokens: 22000},
			},
		},
	}
	store.CreateRun(run5)

	// Add some sample logs
	for _, runID := range []string{"demo-1", "demo-2", "demo-3", "demo-4", "demo-5"} {
		store.AddLog(dashboard.LogEntry{
			Timestamp: now.Add(-30 * time.Minute),
			Level:     "info",
			Message:   "Migration started",
			RunID:     runID,
		})
		store.AddLog(dashboard.LogEntry{
			Timestamp: now.Add(-25 * time.Minute),
			Level:     "info",
			Message:   "Cartographer: parsed source files",
			RunID:     runID,
			Stage:     "cartographer",
		})
		store.AddLog(dashboard.LogEntry{
			Timestamp: now.Add(-20 * time.Minute),
			Level:     "info",
			Message:   "Specular: extracted business rules",
			RunID:     runID,
			Stage:     "specular",
		})
	}
}

// RegisterDashboardCommands registers dashboard and explore subcommands.
func RegisterDashboardCommands(parent *cobra.Command) {
	var (
		dashboardPort int
		dashboardDemo bool
	)
	dashboardCmd := &cobra.Command{
		Use:   "dashboard",
		Short: "Start the Anvil dashboard server",
		RunE: func(cmd *cobra.Command, args []string) error {
			return RunDashboard(dashboardPort, dashboardDemo)
		},
	}
	dashboardCmd.Flags().IntVarP(&dashboardPort, "port", "p", 9090, "Dashboard listen port")
	dashboardCmd.Flags().BoolVar(&dashboardDemo, "demo", false, "Seed with demo migration data")

	var (
		explorePort int
		exploreDirs []string
		exploreOpen bool
	)
	exploreCmd := &cobra.Command{
		Use:   "explore [dirs...]",
		Short: "Start the Proof Pack Explorer server",
		RunE: func(cmd *cobra.Command, args []string) error {
			// Use args as dirs if provided, otherwise use flag value
			if len(args) > 0 {
				exploreDirs = args
			}
			return RunExplore(explorePort, exploreDirs, exploreOpen)
		},
	}
	exploreCmd.Flags().IntVarP(&explorePort, "port", "p", 9091, "Explorer listen port")
	exploreCmd.Flags().StringSliceVar(&exploreDirs, "dirs", []string{"."}, "Directories to scan for proof packs")
	exploreCmd.Flags().BoolVarP(&exploreOpen, "open", "o", false, "Print URL (reserved for future auto-open)")

	parent.AddCommand(dashboardCmd, exploreCmd)
}
