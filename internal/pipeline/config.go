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

	// GraphURI is the Neo4j connection URI for optional semantic graph storage.
	// Format: "neo4j://user:password@host:7687" or split into GraphUsername/GraphPassword.
	// Leave empty to skip Neo4j storage.
	GraphURI      string
	GraphUsername string
	GraphPassword string

	// VectorHost is the Qdrant host for optional function embedding storage.
	// Leave empty to skip Qdrant integration.
	VectorHost       string
	VectorPort       int
	VectorCollection string
}
