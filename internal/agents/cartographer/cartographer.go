package cartographer

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// Cartographer parses source files into the IR, stores the graph, and indexes vectors.
type Cartographer struct{}

func New() *Cartographer { return &Cartographer{} }

func (c *Cartographer) Name() string { return "cartographer" }

func (c *Cartographer) Run(ctx context.Context, ac *agents.AgentContext) (*agents.AgentResult, error) {
	sourceLang := ac.Params["source"]
	inputPath := ac.Params["input"]

	src, err := ac.Registry.Source(sourceLang)
	if err != nil {
		return nil, err
	}

	files, err := loadSourceFiles(inputPath)
	if err != nil {
		return nil, fmt.Errorf("loading source files: %w", err)
	}

	graph, err := src.Parse(ctx, files)
	if err != nil {
		return nil, fmt.Errorf("parsing: %w", err)
	}

	if err := src.ResolveDependencies(ctx, graph); err != nil {
		return nil, fmt.Errorf("resolving dependencies: %w", err)
	}

	if ac.GraphDB != nil {
		if err := ac.GraphDB.StoreGraph(ctx, graph); err != nil {
			return nil, fmt.Errorf("storing graph: %w", err)
		}
	}

	return &agents.AgentResult{Graph: graph}, nil
}

func loadSourceFiles(path string) ([]plugins.SourceFile, error) {
	info, err := os.Stat(path)
	if err != nil {
		return nil, err
	}

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
		ext := filepath.Ext(p)
		if ext == ".cbl" || ext == ".cob" || ext == ".cpy" {
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
