package cli

import (
	"encoding/json"
	"fmt"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/snapshot"
	"github.com/spf13/cobra"
)

// ListSnapshots lists all snapshots in the store.
func ListSnapshots(storeDir string) error {
	store, err := snapshot.NewStore(storeDir)
	if err != nil {
		return fmt.Errorf("open snapshot store: %w", err)
	}

	snapshots := store.List()
	if len(snapshots) == 0 {
		fmt.Println("No snapshots found.")
		return nil
	}

	// Print table header
	fmt.Printf("%-12s %-20s %-20s %-15s %-8s %-10s %-6s\n", "ID", "TAG", "CREATED", "SOURCE->TARGET", "SCORE", "STATUS", "FILES")
	fmt.Println(strings.Repeat("-", 100))

	// Print each snapshot
	for _, s := range snapshots {
		id := s.ID
		if len(id) > 12 {
			id = id[:12]
		}
		tag := s.Tag
		if tag == "" {
			tag = "-"
		}
		if len(tag) > 20 {
			tag = tag[:20]
		}
		created := s.CreatedAt.Format("2006-01-02 15:04:05")
		langPair := fmt.Sprintf("%s->%s", s.SourceLang, s.TargetLang)
		score := fmt.Sprintf("%.2f", s.Score)
		status := s.Status
		if status == "" {
			status = "-"
		}

		fmt.Printf("%-12s %-20s %-20s %-15s %-8s %-10s %-6d\n",
			id, tag, created, langPair, score, status, s.FileCount)
	}

	return nil
}

// ShowSnapshot shows detailed information about a snapshot.
func ShowSnapshot(storeDir, id string) error {
	store, err := snapshot.NewStore(storeDir)
	if err != nil {
		return fmt.Errorf("open snapshot store: %w", err)
	}

	snap, err := store.Load(id)
	if err != nil {
		return fmt.Errorf("load snapshot: %w", err)
	}

	files, err := store.LoadFiles(snap)
	if err != nil {
		return fmt.Errorf("load snapshot files: %w", err)
	}

	fmt.Printf("Snapshot: %s\n", snap.ID)
	fmt.Printf("Created:  %s\n", snap.CreatedAt.Format("2006-01-02 15:04:05"))
	if snap.Tag != "" {
		fmt.Printf("Tag:      %s\n", snap.Tag)
	}
	if snap.ParentID != "" {
		fmt.Printf("Parent:   %s\n", snap.ParentID)
	}
	fmt.Printf("Source:   %s\n", snap.SourceLang)
	fmt.Printf("Target:   %s\n", snap.TargetLang)
	fmt.Printf("Score:    %.2f\n", snap.Score)
	fmt.Printf("Status:   %s\n", snap.Status)

	if snap.Provenance != nil {
		fmt.Printf("\nProvenance:\n")
		fmt.Printf("  Provider:        %s\n", snap.Provenance.Provider)
		fmt.Printf("  Model:           %s\n", snap.Provenance.Model)
		fmt.Printf("  Total LLM Calls: %d\n", snap.Provenance.TotalLLMCalls)
		fmt.Printf("  Total Tokens:    %d\n", snap.Provenance.TotalTokens)
		fmt.Printf("  Total Duration:  %s\n", snap.Provenance.TotalDuration)
		if len(snap.Provenance.AgentOverrides) > 0 {
			fmt.Printf("  Agent Overrides:\n")
			for k, v := range snap.Provenance.AgentOverrides {
				fmt.Printf("    %s: %s\n", k, v)
			}
		}
	}

	if len(snap.AgentStages) > 0 {
		fmt.Printf("\nAgent Stages:\n")
		for _, stage := range snap.AgentStages {
			fmt.Printf("  - %s: %s\n", stage.Name, stage.Status)
			fmt.Printf("    Duration:         %s\n", stage.Duration)
			fmt.Printf("    Score:            %.2f\n", stage.Score)
			fmt.Printf("    LLM Calls:        %d\n", stage.LLMCalls)
			fmt.Printf("    Prompt Tokens:    %d\n", stage.PromptTokens)
			fmt.Printf("    Completion Tokens: %d\n", stage.CompletionTokens)
			if stage.Model != "" {
				fmt.Printf("    Model:            %s\n", stage.Model)
			}
			if stage.ErrorCount > 0 {
				fmt.Printf("    Errors:           %d\n", stage.ErrorCount)
			}
		}
	}

	fmt.Printf("\nFiles (%d):\n", len(files))
	for _, f := range files {
		fmt.Printf("  %s (%d bytes)\n", f.Path, len(f.Content))
	}

	return nil
}

// TagSnapshot tags a snapshot.
func TagSnapshot(storeDir, id, tag string) error {
	store, err := snapshot.NewStore(storeDir)
	if err != nil {
		return fmt.Errorf("open snapshot store: %w", err)
	}

	if err := store.Tag(id, tag); err != nil {
		return fmt.Errorf("tag snapshot: %w", err)
	}

	fmt.Printf("Tagged snapshot %s as '%s'\n", id, tag)
	return nil
}

// DeleteSnapshot deletes a snapshot.
func DeleteSnapshot(storeDir, id string) error {
	store, err := snapshot.NewStore(storeDir)
	if err != nil {
		return fmt.Errorf("open snapshot store: %w", err)
	}

	if err := store.Delete(id); err != nil {
		return fmt.Errorf("delete snapshot: %w", err)
	}

	fmt.Printf("Deleted snapshot %s\n", id)
	return nil
}

// DiffSnapshots compares two snapshots.
func DiffSnapshots(storeDir, id1, id2 string, jsonOutput bool) error {
	store, err := snapshot.NewStore(storeDir)
	if err != nil {
		return fmt.Errorf("open snapshot store: %w", err)
	}

	snap1, err := store.Load(id1)
	if err != nil {
		return fmt.Errorf("load snapshot %s: %w", id1, err)
	}

	snap2, err := store.Load(id2)
	if err != nil {
		return fmt.Errorf("load snapshot %s: %w", id2, err)
	}

	diff, err := snapshot.Diff(snap1, snap2, store)
	if err != nil {
		return fmt.Errorf("diff snapshots: %w", err)
	}

	if jsonOutput {
		data, err := json.MarshalIndent(diff, "", "  ")
		if err != nil {
			return fmt.Errorf("marshal diff: %w", err)
		}
		fmt.Println(string(data))
	} else {
		fmt.Print(snapshot.FormatDiff(diff))
	}

	return nil
}

// RollbackSnapshot restores files from a snapshot.
func RollbackSnapshot(storeDir, id, outputDir string) error {
	store, err := snapshot.NewStore(storeDir)
	if err != nil {
		return fmt.Errorf("open snapshot store: %w", err)
	}

	snap, err := store.Load(id)
	if err != nil {
		return fmt.Errorf("load snapshot: %w", err)
	}

	if err := store.Restore(snap, outputDir); err != nil {
		return fmt.Errorf("restore snapshot: %w", err)
	}

	fmt.Printf("Restored snapshot %s to %s\n", id, outputDir)
	return nil
}

// RegisterSnapshotCommands registers all snapshot subcommands.
func RegisterSnapshotCommands(parent *cobra.Command) {
	snapshotCmd := &cobra.Command{
		Use:   "snapshot",
		Short: "Manage migration snapshots",
	}

	var snapshotStoreDir string

	// snapshot list
	snapshotListCmd := &cobra.Command{
		Use:   "list",
		Short: "List all snapshots",
		RunE: func(cmd *cobra.Command, args []string) error {
			return ListSnapshots(snapshotStoreDir)
		},
	}
	snapshotListCmd.Flags().StringVar(&snapshotStoreDir, "store-dir", ".anvil-snapshots", "Snapshot store directory")

	// snapshot show
	snapshotShowCmd := &cobra.Command{
		Use:   "show <id>",
		Short: "Show snapshot details",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return ShowSnapshot(snapshotStoreDir, args[0])
		},
	}
	snapshotShowCmd.Flags().StringVar(&snapshotStoreDir, "store-dir", ".anvil-snapshots", "Snapshot store directory")

	// snapshot tag
	snapshotTagCmd := &cobra.Command{
		Use:   "tag <id> <tag>",
		Short: "Tag a snapshot",
		Args:  cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			return TagSnapshot(snapshotStoreDir, args[0], args[1])
		},
	}
	snapshotTagCmd.Flags().StringVar(&snapshotStoreDir, "store-dir", ".anvil-snapshots", "Snapshot store directory")

	// snapshot delete
	snapshotDeleteCmd := &cobra.Command{
		Use:   "delete <id>",
		Short: "Delete a snapshot",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return DeleteSnapshot(snapshotStoreDir, args[0])
		},
	}
	snapshotDeleteCmd.Flags().StringVar(&snapshotStoreDir, "store-dir", ".anvil-snapshots", "Snapshot store directory")

	// snapshot diff
	var snapshotDiffJSON bool
	snapshotDiffCmd := &cobra.Command{
		Use:   "diff <id1> <id2>",
		Short: "Diff two snapshots",
		Args:  cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			return DiffSnapshots(snapshotStoreDir, args[0], args[1], snapshotDiffJSON)
		},
	}
	snapshotDiffCmd.Flags().StringVar(&snapshotStoreDir, "store-dir", ".anvil-snapshots", "Snapshot store directory")
	snapshotDiffCmd.Flags().BoolVar(&snapshotDiffJSON, "json", false, "Output diff as JSON")

	// snapshot rollback
	var snapshotRollbackOutput string
	snapshotRollbackCmd := &cobra.Command{
		Use:   "rollback <id>",
		Short: "Restore files from a snapshot",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return RollbackSnapshot(snapshotStoreDir, args[0], snapshotRollbackOutput)
		},
	}
	snapshotRollbackCmd.Flags().StringVar(&snapshotStoreDir, "store-dir", ".anvil-snapshots", "Snapshot store directory")
	snapshotRollbackCmd.Flags().StringVar(&snapshotRollbackOutput, "output", "", "Output directory for restored files")
	_ = snapshotRollbackCmd.MarkFlagRequired("output")

	snapshotCmd.AddCommand(snapshotListCmd, snapshotShowCmd, snapshotTagCmd, snapshotDeleteCmd, snapshotDiffCmd, snapshotRollbackCmd)

	parent.AddCommand(snapshotCmd)
}
