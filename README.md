<div align="center">

# 🔨 Anvil

**Multi-Agent Legacy Code Modernization Platform**

Transform legacy codebases into modern, cloud-native applications using AI-powered agents.

[![Go](https://img.shields.io/badge/Go-1.23+-00ADD8?style=for-the-badge&logo=go&logoColor=white)](https://go.dev)
[![Temporal](https://img.shields.io/badge/Temporal-Orchestration-7B61FF?style=for-the-badge)](https://temporal.io)
[![License](https://img.shields.io/badge/License-MIT-green?style=for-the-badge)](LICENSE)

---

**COBOL → Java Spring Boot** as the first supported pipeline.

More source/target pairs coming soon.

</div>

## Overview

Anvil is a pluggable, orchestrated platform that uses a pipeline of specialized AI agents to understand, decompose, and rewrite legacy code into modern equivalents — while preserving business logic and semantics.

```
┌─────────────┐     ┌───────────┐     ┌───────────┐     ┌─────────┐
│ Cartographer │────▶│  Specular  │────▶│ Architect  │────▶│  Judge  │
│  Parse → IR  │     │ Rules+LLM │     │ IR → Java  │     │ Verify  │
└─────────────┘     └───────────┘     └───────────┘     └────┬────┘
                                            ▲                 │
                                            └─── retry (2x) ──┘
```

### Agents

| Agent | Role |
|---|---|
| **Cartographer** | Parses source files into a language-agnostic Intermediate Representation (IR), builds call graphs, indexes vectors |
| **Specular** | Uses LLM to extract business rules, annotate complexity, and enrich the IR |
| **Architect** | Generates target language code from the enriched IR using LLM + templates |
| **Judge** | Verifies semantic equivalence between source and generated code, triggers retries on failure |

## Architecture

```
cmd/
  anvil/          CLI entry point (cobra)
  worker/         Temporal worker binary

internal/
  ir/             Intermediate Representation structs
  llm/            LLM provider interface + implementations (Anthropic, OpenAI/vLLM)
  agents/         Agent interface + 4 agent implementations
  plugins/        Source/Target plugin interfaces + registry
    source/cobol/ COBOL parser (data division, procedure division, copybooks)
    target/java/  Java Spring Boot generator (class mapper, type mapper, scaffolding)
  graph/          Graph storage interface + Neo4j implementation
  vector/         Vector storage interface + Qdrant implementation
  temporal/       Workflow orchestration (workflows, activities, worker)
  config/         Configuration (Viper)

pkg/
  treesitter/     Tree-sitter Go wrapper for source parsing

configs/
  anvil.yaml      Default configuration

testdata/
  cobol/          Sample COBOL programs for testing
```

## Quick Start

### Prerequisites

- Go 1.23+
- Neo4j (for graph storage)
- Qdrant (for vector storage)
- Temporal (for workflow orchestration)
- An LLM API key (Anthropic or OpenAI)

### Install

```bash
git clone https://github.com/efebarandurmaz/anvil.git
cd anvil
go mod download
make build
```

### Configure

```bash
cp configs/anvil.yaml configs/local.yaml
```

Edit `configs/local.yaml` with your API keys and service endpoints:

```yaml
llm:
  provider: anthropic          # or "openai"
  model: claude-sonnet-4-20250514
  api_key: "sk-..."

graph:
  uri: bolt://localhost:7687
  username: neo4j
  password: password

vector:
  host: localhost
  port: 6334
  collection: anvil

temporal:
  host: localhost:7233
  namespace: default
  task_queue: anvil-tasks
```

### Run

**Direct mode** (no Temporal required):

```bash
./anvil run \
  --source cobol \
  --target java \
  --input testdata/cobol/calculator.cbl \
  --output /tmp/anvil-out \
  --config configs/local.yaml
```

**With Temporal orchestration:**

```bash
# Terminal 1: Start the worker
./worker configs/local.yaml

# Terminal 2: Submit a modernization job
./anvil run \
  --source cobol \
  --target java \
  --input testdata/cobol/calculator.cbl \
  --output /tmp/anvil-out
```

### Test

```bash
make test
```

## Supported Languages

| Source | Target | Status |
|--------|--------|--------|
| COBOL  | Java Spring Boot | ✅ MVP |
| PL/I   | Java / Kotlin    | Planned |
| RPG    | Python / Java    | Planned |

## LLM Providers

| Provider | Completion | Embedding |
|----------|-----------|-----------|
| Anthropic (Claude) | ✅ | ❌ (use OpenAI for embeddings) |
| OpenAI | ✅ | ✅ |
| vLLM (OpenAI-compatible) | ✅ | ✅ |

Set `llm.base_url` in config to point to any OpenAI-compatible endpoint (vLLM, Ollama, etc.).

## Plugin System

Anvil is designed around pluggable source and target languages:

```go
// Implement SourcePlugin to add a new source language
type SourcePlugin interface {
    Language() string
    Parse(ctx context.Context, files []SourceFile) (*ir.SemanticGraph, error)
    ResolveDependencies(ctx context.Context, graph *ir.SemanticGraph) error
}

// Implement TargetPlugin to add a new target language
type TargetPlugin interface {
    Language() string
    Generate(ctx context.Context, graph *ir.SemanticGraph, provider llm.Provider) ([]GeneratedFile, error)
    Scaffold(ctx context.Context, graph *ir.SemanticGraph) ([]GeneratedFile, error)
}
```

Register your plugin in the registry and it's automatically available in the pipeline.

## Environment Variables

All config values can be overridden via environment variables with the `ANVIL_` prefix:

```bash
export ANVIL_LLM_API_KEY="sk-..."
export ANVIL_LLM_PROVIDER="openai"
export ANVIL_GRAPH_URI="bolt://localhost:7687"
```

## License

MIT
