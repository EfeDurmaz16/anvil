package pipeline

import "github.com/efebarandurmaz/anvil/internal/plugins"

// PipelineResult contains the output of a pipeline run.
type PipelineResult struct {
	Files      []plugins.GeneratedFile
	Score      float64
	Errors     []string
	Iterations int
}
