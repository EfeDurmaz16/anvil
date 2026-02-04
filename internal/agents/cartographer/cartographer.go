package cartographer

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/agents"
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
