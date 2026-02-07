package cartographer

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/migration"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// Cartographer parses source files into the IR, stores the graph, and indexes vectors.
type Cartographer struct{}

func New() *Cartographer { return &Cartographer{} }

func (c *Cartographer) Name() string { return "cartographer" }

func (c *Cartographer) Run(ctx context.Context, ac *agents.AgentContext) (*agents.AgentResult, error) {
	result := agents.NewAgentResult()

	sourceLang := ac.Params["source"]
	inputPath := ac.Params["input"]
	result.Metadata["source_language"] = sourceLang
	result.Metadata["input_path"] = inputPath

	src, err := ac.Registry.Source(sourceLang)
	if err != nil {
		result.Status = agents.StatusFailed
		result.AddError(fmt.Sprintf("registry lookup: %v", err))
		result.Finalize()
		return result, err
	}

	files, err := loadSourceFiles(inputPath, src)
	if err != nil {
		result.Status = agents.StatusFailed
		result.AddError(fmt.Sprintf("loading source files: %v", err))
		result.Finalize()
		return result, fmt.Errorf("loading source files: %w", err)
	}
	result.Metrics.InputItems = len(files)

	graph, err := src.Parse(ctx, files)
	if err != nil {
		result.Status = agents.StatusFailed
		result.AddError(fmt.Sprintf("parsing: %v", err))
		result.Finalize()
		return result, fmt.Errorf("parsing: %w", err)
	}

	if err := src.ResolveDependencies(ctx, graph); err != nil {
		result.Status = agents.StatusFailed
		result.AddError(fmt.Sprintf("resolving dependencies: %v", err))
		result.Finalize()
		return result, fmt.Errorf("resolving dependencies: %w", err)
	}

	if ac.GraphDB != nil {
		if err := ac.GraphDB.StoreGraph(ctx, graph); err != nil {
			result.AddWarning(fmt.Sprintf("storing graph: %v", err))
			// Non-fatal: continue without graph storage
		}
	}

	result.Graph = graph

	// Compute fingerprints for incremental migration support
	deps := buildDependencyMap(graph)
	fingerprints := migration.ComputeFingerprints(files, deps)
	fpCount := len(fingerprints)

	// Store fingerprint summary in metadata
	result.Metadata["fingerprints_computed"] = fmt.Sprintf("%d", fpCount)

	// Store state if output path is provided
	if outputPath := ac.Params["output"]; outputPath != "" {
		state := migration.NewMigrationState(sourceLang, ac.Params["target"])
		state.Fingerprints = fingerprints
		if err := state.Save(outputPath); err != nil {
			result.AddWarning(fmt.Sprintf("failed to save migration state: %v", err))
		}
	}

	result.Status = agents.StatusSuccess

	// Count output items (modules and functions)
	moduleCount := len(graph.Modules)
	funcCount := 0
	for _, mod := range graph.Modules {
		funcCount += len(mod.Functions)
	}
	result.Metrics.OutputItems = funcCount
	result.Metadata["modules_parsed"] = fmt.Sprintf("%d", moduleCount)
	result.Metadata["functions_parsed"] = fmt.Sprintf("%d", funcCount)

	result.Finalize()
	return result, nil
}

func loadSourceFiles(path string, src plugins.SourcePlugin) ([]plugins.SourceFile, error) {
	info, err := os.Stat(path)
	if err != nil {
		return nil, err
	}

	allowed := extensionSet(src)

	var files []plugins.SourceFile
	if !info.IsDir() {
		data, err := os.ReadFile(path)
		if err != nil {
			return nil, err
		}
		files = append(files, plugins.SourceFile{Path: path, Content: data})

		// Auto-discover copybook files for single-file input.
		// Search sibling directories (e.g., ../cpy/) and same directory for .cpy files.
		cpyFiles := discoverCopybooks(path)
		files = append(files, cpyFiles...)
		return files, nil
	}

	err = filepath.Walk(path, func(p string, fi os.FileInfo, err error) error {
		if err != nil || fi.IsDir() {
			return err
		}
		if len(allowed) == 0 || allowed[strings.ToLower(filepath.Ext(p))] {
			data, err := os.ReadFile(p)
			if err != nil {
				return err
			}
			files = append(files, plugins.SourceFile{Path: p, Content: data})
		}
		return nil
	})
	return files, err
}

func extensionSet(src plugins.SourcePlugin) map[string]bool {
	fep, ok := src.(plugins.FileExtensionsProvider)
	if !ok {
		return map[string]bool{
			".cbl": true,
			".cob": true,
			".cpy": true,
		}
	}
	out := make(map[string]bool)
	for _, ext := range fep.FileExtensions() {
		ext = strings.TrimSpace(strings.ToLower(ext))
		if ext == "" {
			continue
		}
		if ext[0] != '.' {
			ext = "." + ext
		}
		out[ext] = true
	}
	return out
}

// discoverCopybooks searches for .cpy files near a single-file input.
// It checks the same directory and common sibling directories (../cpy/, ../copy/).
func discoverCopybooks(sourcePath string) []plugins.SourceFile {
	dir := filepath.Dir(sourcePath)
	parent := filepath.Dir(dir)

	searchDirs := []string{
		dir,                          // same directory
		filepath.Join(parent, "cpy"), // sibling cpy/ (CardDemo layout)
		filepath.Join(parent, "copy"),
		filepath.Join(dir, "cpy"),
		filepath.Join(dir, "copy"),
	}

	seen := make(map[string]bool)
	var result []plugins.SourceFile

	for _, d := range searchDirs {
		entries, err := os.ReadDir(d)
		if err != nil {
			continue
		}
		for _, e := range entries {
			if e.IsDir() {
				continue
			}
			ext := strings.ToLower(filepath.Ext(e.Name()))
			if ext != ".cpy" {
				continue
			}
			full := filepath.Join(d, e.Name())
			if seen[full] {
				continue
			}
			seen[full] = true
			data, err := os.ReadFile(full)
			if err != nil {
				continue
			}
			result = append(result, plugins.SourceFile{Path: full, Content: data})
		}
	}
	return result
}

// buildDependencyMap extracts dependency relationships from the graph.
// Currently maps modules to their copybook/include dependencies.
func buildDependencyMap(graph *ir.SemanticGraph) map[string][]string {
	deps := make(map[string][]string)

	for _, mod := range graph.Modules {
		if mod.Metadata == nil {
			continue
		}
		// Check for copy_refs metadata (set by COBOL parser)
		if refs, ok := mod.Metadata["copy_refs"]; ok && refs != "" {
			for _, ref := range strings.Split(refs, ",") {
				ref = strings.TrimSpace(ref)
				if ref != "" {
					deps[mod.Path] = append(deps[mod.Path], ref)
				}
			}
		}
		// Check for copybooks_resolved metadata
		if resolved, ok := mod.Metadata["copybooks_resolved"]; ok && resolved != "" {
			for _, name := range strings.Split(resolved, ",") {
				name = strings.TrimSpace(name)
				if name != "" {
					deps[mod.Path] = append(deps[mod.Path], name+".cpy")
				}
			}
		}
	}

	return deps
}
