package pipeline

import (
	"github.com/efebarandurmaz/anvil/internal/config"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// PipelineConfig contains all configuration for running a migration pipeline.
type PipelineConfig struct {
	SourceLang string
	TargetLang string
	InputPath  string
	OutputPath string
	Config     *config.Config
	Registry   *plugins.Registry
	Providers  map[string]llm.Provider
	JsonReport bool
}
