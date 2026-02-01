"use client";

import { useState } from "react";
import { Github, Sun, Moon, Search, ChevronRight, ArrowLeft, ArrowRight, AlertTriangle, Zap, Puzzle, Bot, Workflow } from "lucide-react";
import { useTheme } from "@/components/ThemeProvider";

const sidebarNav = [
  {
    group: "Getting Started",
    items: [
      { label: "Introduction", slug: "introduction", active: true },
      { label: "Installation", slug: "installation" },
      { label: "Quick Start", slug: "quick-start" },
      { label: "Configuration", slug: "configuration" },
    ],
  },
  {
    group: "Pipeline",
    items: [
      { label: "Cartographer", slug: "cartographer" },
      { label: "Specular", slug: "specular" },
      { label: "Architect", slug: "architect" },
      { label: "Judge", slug: "judge" },
    ],
  },
  {
    group: "Architecture",
    items: [
      { label: "Semantic Graph", slug: "semantic-graph" },
      { label: "Plugin System", slug: "plugin-system" },
      { label: "LLM Providers", slug: "llm-providers" },
    ],
  },
  {
    group: "Reference",
    items: [
      { label: "CLI Reference", slug: "cli-reference" },
      { label: "Config File", slug: "config-file" },
      { label: "API Reference", slug: "api-reference" },
      { label: "Contributing", slug: "contributing" },
    ],
  },
];

const pages: Record<string, React.ReactNode> = {
  introduction: <IntroductionPage />,
  installation: <InstallationPage />,
  "quick-start": <QuickStartPage />,
  configuration: <ConfigurationPage />,
  cartographer: <AgentPage agent="Cartographer" num="01" role="Parse & Map" desc="The Cartographer agent reads COBOL source files and builds a complete semantic graph — modules, functions, data types, call graphs, control flow, and I/O contracts. It uses tree-sitter for parsing and requires no LLM." details={["Parses fixed-format COBOL (columns 1-72) with Area A/B detection", "Extracts IDENTIFICATION, DATA, and PROCEDURE divisions", "Builds call graphs from PERFORM, CALL, and GO TO statements", "Maps all PIC clauses to typed IR nodes (PIC 9 → numeric, PIC X → string)", "Resolves COPY/INCLUDE copybook dependencies", "Detects EXEC CICS and EXEC SQL blocks for I/O classification", "Stores the semantic graph in Neo4j and generates vector embeddings in Qdrant"]} />,
  specular: <AgentPage agent="Specular" num="02" role="Extract Business Rules" desc="The Specular agent uses an LLM to analyze the semantic graph and extract business rules, validation logic, and domain patterns that aren't visible from syntax alone." details={["Queries the semantic graph for complex conditional logic", "Uses LLM to identify business rules embedded in EVALUATE/IF chains", "Extracts validation patterns (field-level, cross-field, temporal)", "Documents domain-specific terminology and naming conventions", "Scores complexity per function to prioritize review", "Enriches the IR with business rule annotations", "Works without LLM in template-only mode (rules are skipped)"]} />,
  architect: <AgentPage agent="Architect" num="03" role="Generate Modern Code" desc="The Architect agent transforms the enriched IR into idiomatic Java Spring Boot code with proper type mapping, Spring annotations, and clean architecture patterns." details={["Maps COBOL data types to Java types (PIC 9→int/long/BigDecimal, PIC X→String)", "Generates @Service, @RestController, @Repository classes", "Creates Spring Boot project structure (controller/service/model/repository)", "Uses LLM for complex logic translation when available", "Falls back to template-based generation in offline mode", "Generates unit test stubs alongside production code", "Preserves original COBOL comments as Javadoc annotations"]} />,
  judge: <AgentPage agent="Judge" num="04" role="Verify Equivalence" desc="The Judge agent validates that generated Java code preserves the original COBOL business semantics. If verification fails, the Architect retries automatically (up to 3 times)." details={["Compares input/output contracts between COBOL and Java", "Verifies type mapping correctness (no precision loss)", "Checks that all business rules are preserved in generated code", "Runs generated Java through compilation check", "Scores semantic equivalence on a 0-1 scale (threshold: 0.85)", "On failure, provides detailed feedback to Architect for retry", "Retry loop runs up to 3 times before reporting partial success"]} />,
  "semantic-graph": <PlaceholderPage title="Semantic Graph" desc="The Semantic Graph (IR) is Anvil's central data structure. It represents the complete understanding of a COBOL codebase as a typed, queryable graph of modules, functions, data types, call edges, and I/O contracts." />,
  "plugin-system": <PlaceholderPage title="Plugin System" desc="Anvil's plugin architecture allows adding new source languages and target platforms. Implement the SourcePlugin or TargetPlugin interface to extend Anvil beyond COBOL → Java." />,
  "llm-providers": <PlaceholderPage title="LLM Providers" desc="Anvil supports multiple LLM providers through a unified interface: Anthropic (Claude), OpenAI (GPT-4), Groq, Ollama, and vLLM. Configure your provider in anvil.yaml or run fully offline with template-only mode." />,
  "cli-reference": <CLIReferencePage />,
  "config-file": <PlaceholderPage title="Config File" desc="Anvil is configured via anvil.yaml. All settings have sensible defaults — you can run Anvil with zero configuration for offline template-based generation." />,
  "api-reference": <PlaceholderPage title="API Reference" desc="Anvil exposes a Go API for programmatic access. Import the packages directly to embed Anvil in your own tools and workflows." />,
  contributing: <PlaceholderPage title="Contributing" desc="Anvil is open source under the MIT license. We welcome contributions of all kinds — bug reports, feature requests, documentation improvements, and code contributions." />,
};

export default function DocsPage() {
  const { theme, toggle } = useTheme();
  const [activePage, setActivePage] = useState("introduction");

  const allItems = sidebarNav.flatMap((g) => g.items);
  const currentIndex = allItems.findIndex((i) => i.slug === activePage);
  const prev = currentIndex > 0 ? allItems[currentIndex - 1] : null;
  const next = currentIndex < allItems.length - 1 ? allItems[currentIndex + 1] : null;
  const currentGroup = sidebarNav.find((g) => g.items.some((i) => i.slug === activePage));

  return (
    <div className="min-h-screen transition-colors" style={{ backgroundColor: "var(--color-bg)" }}>
      {/* Header */}
      <header
        className="sticky top-0 z-50 flex items-center justify-between px-6 py-3 backdrop-blur-md transition-colors"
        style={{ backgroundColor: "var(--color-header-bg)", borderBottom: "1px solid var(--color-border)" }}
      >
        <div className="flex items-center gap-4">
          <a href="/" className="flex items-center gap-2">
            <div className="w-6 h-6 bg-[var(--color-accent)] rounded-sm flex items-center justify-center">
              <span className="text-black font-mono text-[10px] font-bold">A</span>
            </div>
            <span className="font-mono text-sm font-bold tracking-[2px]" style={{ color: "var(--color-text)" }}>ANVIL</span>
          </a>
          <span className="font-mono text-[11px] px-2 py-0.5" style={{ backgroundColor: "var(--color-surface)", color: "var(--color-text-secondary)", border: "1px solid var(--color-border)" }}>
            Docs
          </span>
        </div>

        <div className="flex items-center gap-3">
          <div className="flex items-center gap-2 px-3 py-1.5" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-surface)" }}>
            <Search size={14} style={{ color: "var(--color-text-muted)" }} />
            <span className="font-mono text-[11px]" style={{ color: "var(--color-text-muted)" }}>Search docs...</span>
            <span className="font-mono text-[10px] px-1.5 py-0.5 ml-8" style={{ backgroundColor: "var(--color-bg)", color: "var(--color-text-muted)", border: "1px solid var(--color-border)" }}>
              &#8984;K
            </span>
          </div>
          <button onClick={toggle} className="p-2 transition-opacity hover:opacity-80" style={{ color: "var(--color-text-secondary)" }}>
            {theme === "dark" ? <Sun size={16} /> : <Moon size={16} />}
          </button>
          <a href="https://github.com/EfeDurmaz16/anvil" target="_blank" rel="noopener noreferrer" className="p-2 transition-opacity hover:opacity-80" style={{ color: "var(--color-text-secondary)" }}>
            <Github size={16} />
          </a>
        </div>
      </header>

      <div className="flex">
        {/* Sidebar */}
        <aside className="w-[260px] shrink-0 sticky top-[53px] h-[calc(100vh-53px)] overflow-y-auto p-5 flex flex-col gap-6" style={{ borderRight: "1px solid var(--color-border)", backgroundColor: "var(--color-surface-alt)" }}>
          {sidebarNav.map((group) => (
            <div key={group.group} className="flex flex-col gap-1">
              <span className="font-mono text-[10px] font-semibold tracking-[2px] mb-1" style={{ color: "var(--color-text-muted)" }}>
                {group.group.toUpperCase()}
              </span>
              {group.items.map((item) => (
                <button
                  key={item.slug}
                  onClick={() => setActivePage(item.slug)}
                  className="text-left font-mono text-[13px] px-3 py-1.5 transition-colors"
                  style={{
                    color: activePage === item.slug ? "var(--color-accent)" : "var(--color-text-secondary)",
                    backgroundColor: activePage === item.slug ? "var(--color-accent-dim)" : "transparent",
                    borderLeft: activePage === item.slug ? "2px solid var(--color-accent)" : "2px solid transparent",
                  }}
                >
                  {item.label}
                </button>
              ))}
            </div>
          ))}
        </aside>

        {/* Main content */}
        <main className="flex-1 max-w-[800px] px-12 py-8 flex flex-col gap-8">
          {/* Breadcrumbs */}
          <div className="flex items-center gap-1.5 font-mono text-[11px]">
            <a href="/" className="hover:opacity-80 transition-opacity" style={{ color: "var(--color-text-muted)" }}>Home</a>
            <ChevronRight size={12} style={{ color: "var(--color-text-muted)" }} />
            <span style={{ color: "var(--color-text-muted)" }}>Docs</span>
            {currentGroup && (
              <>
                <ChevronRight size={12} style={{ color: "var(--color-text-muted)" }} />
                <span style={{ color: "var(--color-text-muted)" }}>{currentGroup.group}</span>
              </>
            )}
            <ChevronRight size={12} style={{ color: "var(--color-text-muted)" }} />
            <span style={{ color: "var(--color-text-secondary)" }}>{allItems[currentIndex]?.label}</span>
          </div>

          {/* Page content */}
          {pages[activePage]}

          {/* Prev/Next */}
          <div className="flex justify-between mt-8 pt-6" style={{ borderTop: "1px solid var(--color-border)" }}>
            {prev ? (
              <button onClick={() => setActivePage(prev.slug)} className="flex items-center gap-2 font-mono text-sm hover:opacity-80 transition-opacity" style={{ color: "var(--color-text-secondary)" }}>
                <ArrowLeft size={16} /> {prev.label}
              </button>
            ) : <div />}
            {next ? (
              <button onClick={() => setActivePage(next.slug)} className="flex items-center gap-2 font-mono text-sm hover:opacity-80 transition-opacity" style={{ color: "var(--color-accent)" }}>
                {next.label} <ArrowRight size={16} />
              </button>
            ) : <div />}
          </div>

          {/* Footer */}
          <div className="flex items-center justify-between py-6 mt-4" style={{ borderTop: "1px solid var(--color-border)" }}>
            <span className="font-mono text-[11px]" style={{ color: "var(--color-text-muted)" }}>Anvil Documentation</span>
            <div className="flex items-center gap-4">
              <a href="https://github.com/EfeDurmaz16/anvil" className="font-mono text-[11px] hover:opacity-80 transition-opacity" style={{ color: "var(--color-text-muted)" }}>GitHub</a>
              <a href="/" className="font-mono text-[11px] hover:opacity-80 transition-opacity" style={{ color: "var(--color-text-muted)" }}>Home</a>
            </div>
          </div>
        </main>
      </div>
    </div>
  );
}

function IntroductionPage() {
  return (
    <>
      <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>Introduction</h1>
      <p className="font-mono text-sm leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        Anvil is an open-source, multi-agent platform that modernizes legacy COBOL codebases into production-grade Java Spring Boot applications. It uses a pipeline of four specialized AI agents — Cartographer, Specular, Architect, and Judge — to parse, understand, transform, and verify your code.
      </p>

      <h2 className="text-xl font-semibold mt-4" style={{ color: "var(--color-text)" }}>What is Anvil?</h2>
      <p className="font-mono text-sm leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        Legacy mainframe systems running COBOL still power critical infrastructure in banking, insurance, healthcare, and government. These systems are reliable but increasingly difficult to maintain as the workforce that built them retires. Anvil bridges this gap by automatically translating COBOL into modern, idiomatic Java while preserving the original business logic.
      </p>

      {/* Alert */}
      <div className="flex gap-3 p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <AlertTriangle size={18} className="text-[var(--color-accent)] shrink-0 mt-0.5" />
        <div className="flex flex-col gap-1">
          <span className="font-mono text-[12px] font-semibold" style={{ color: "var(--color-text)" }}>LLM is optional</span>
          <span className="font-mono text-[12px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
            Anvil works fully offline with template-based generation. The Cartographer and Architect agents can run without any LLM. Adding an LLM provider (Claude, GPT-4, Ollama) enables business rule extraction via Specular and improves code quality.
          </span>
        </div>
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Key Features</h2>
      <div className="flex flex-col gap-3 mt-2">
        {[
          { icon: Bot, title: "Multi-Agent Pipeline", desc: "Four specialized agents (Cartographer, Specular, Architect, Judge) each handle one step of the modernization process. The pipeline runs sequentially with an automatic retry loop between Architect and Judge." },
          { icon: Puzzle, title: "Pluggable LLM Interface", desc: "Works with Anthropic Claude, OpenAI GPT-4, Groq, Ollama, vLLM, or no LLM at all. Switch providers with a single config change." },
          { icon: Workflow, title: "Production-Grade Orchestration", desc: "Built on Temporal.io for reliable workflow execution with retry logic, observability, and distributed processing. Not a script — a production system." },
          { icon: Zap, title: "Extensible Plugin System", desc: "COBOL → Java today. The plugin interface supports any source and target language. Add RPG, PL/I, Fortran, or any language by implementing the SourcePlugin/TargetPlugin interfaces." },
        ].map(({ icon: Icon, title, desc }) => (
          <div key={title} className="flex gap-3 p-4" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-card-bg)" }}>
            <Icon size={18} className="text-[var(--color-accent)] shrink-0 mt-0.5" />
            <div className="flex flex-col gap-1">
              <span className="font-mono text-[13px] font-semibold" style={{ color: "var(--color-text)" }}>{title}</span>
              <span className="font-mono text-[12px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>{desc}</span>
            </div>
          </div>
        ))}
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Quick Install</h2>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-code-green)" }}>
{`$ go install github.com/efebarandurmaz/anvil/cmd/anvil@latest
$ anvil run --source cobol --target java \\
    --input ./your-cobol-src \\
    --output ./generated-java`}
        </pre>
      </div>
    </>
  );
}

function InstallationPage() {
  return (
    <>
      <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>Installation</h1>
      <p className="font-mono text-sm leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        Anvil requires Go 1.21 or later. It has zero external runtime dependencies for basic operation.
      </p>

      <h2 className="text-xl font-semibold mt-4" style={{ color: "var(--color-text)" }}>Using Go Install</h2>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-code-green)" }}>
{`$ go install github.com/efebarandurmaz/anvil/cmd/anvil@latest`}
        </pre>
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Building from Source</h2>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-code-green)" }}>
{`$ git clone https://github.com/EfeDurmaz16/anvil.git
$ cd anvil
$ make build
$ ./bin/anvil --version`}
        </pre>
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Optional Dependencies</h2>
      <p className="font-mono text-sm leading-relaxed mt-2" style={{ color: "var(--color-text-secondary)" }}>
        For full functionality with knowledge storage and workflow orchestration:
      </p>
      <div className="flex flex-col gap-2 mt-3">
        {[
          { name: "Temporal.io", desc: "Workflow orchestration (required for production pipelines)" },
          { name: "Neo4j", desc: "Graph database for semantic graph storage" },
          { name: "Qdrant", desc: "Vector database for semantic search and embeddings" },
        ].map((dep) => (
          <div key={dep.name} className="flex gap-3 p-3" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-card-bg)" }}>
            <span className="font-mono text-[12px] font-semibold w-24 shrink-0" style={{ color: "var(--color-accent)" }}>{dep.name}</span>
            <span className="font-mono text-[12px]" style={{ color: "var(--color-text-secondary)" }}>{dep.desc}</span>
          </div>
        ))}
      </div>
    </>
  );
}

function QuickStartPage() {
  return (
    <>
      <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>Quick Start</h1>
      <p className="font-mono text-sm leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        Get Anvil running in under a minute. No configuration required for basic offline operation.
      </p>

      <h2 className="text-xl font-semibold mt-4" style={{ color: "var(--color-text)" }}>1. Install Anvil</h2>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-code-green)" }}>
{`$ go install github.com/efebarandurmaz/anvil/cmd/anvil@latest`}
        </pre>
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>2. Run on Your COBOL</h2>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-code-green)" }}>
{`$ anvil run --source cobol --target java \\
    --input ./your-cobol-src \\
    --output ./generated-java`}
        </pre>
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>3. View Results</h2>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-code-green)" }}>
{`$ anvil run --source cobol --target java \\
    --input ./your-cobol-src \\
    --output ./generated-java \\
    --json | jq .`}
        </pre>
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>4. Enable AI Features (Optional)</h2>
      <p className="font-mono text-sm leading-relaxed mt-2" style={{ color: "var(--color-text-secondary)" }}>
        Create an <code className="text-[var(--color-accent)]">anvil.yaml</code> file to enable LLM-powered business rule extraction:
      </p>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-accent)" }}>
{`llm:
  provider: anthropic
  api_key: \${ANTHROPIC_API_KEY}
  model: claude-sonnet-4-20250514`}
        </pre>
      </div>
    </>
  );
}

function ConfigurationPage() {
  return (
    <>
      <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>Configuration</h1>
      <p className="font-mono text-sm leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        Anvil is configured through <code className="text-[var(--color-accent)]">anvil.yaml</code>. All options have sensible defaults.
      </p>

      <h2 className="text-xl font-semibold mt-4" style={{ color: "var(--color-text)" }}>Full Configuration Example</h2>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-accent)" }}>
{`# anvil.yaml
llm:
  provider: anthropic       # anthropic | openai | groq | ollama
  api_key: \${ANTHROPIC_API_KEY}
  model: claude-sonnet-4-20250514
  temperature: 0.1
  max_tokens: 4096

source:
  language: cobol
  encoding: utf-8

target:
  language: java
  framework: spring-boot
  java_version: 17

pipeline:
  max_retries: 3            # Architect-Judge retry limit
  quality_threshold: 0.85   # Judge pass threshold (0-1)

storage:
  neo4j:
    uri: bolt://localhost:7687
    username: neo4j
    password: \${NEO4J_PASSWORD}
  qdrant:
    host: localhost
    port: 6334

temporal:
  host: localhost:7233
  namespace: anvil
  task_queue: modernization`}
        </pre>
      </div>
    </>
  );
}

function AgentPage({ agent, num, role, desc, details }: { agent: string; num: string; role: string; desc: string; details: string[] }) {
  return (
    <>
      <div className="flex items-center gap-3">
        <span className="text-[32px] font-bold text-[var(--color-accent)]">{num}</span>
        <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>{agent}</h1>
      </div>
      <span className="font-mono text-[11px] font-semibold tracking-[2px] text-[var(--color-accent)]">{role.toUpperCase()}</span>
      <p className="font-mono text-sm leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>{desc}</p>

      <h2 className="text-xl font-semibold mt-4" style={{ color: "var(--color-text)" }}>What it does</h2>
      <div className="flex flex-col gap-2 mt-2">
        {details.map((d, i) => (
          <div key={i} className="flex gap-3 p-3" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-card-bg)" }}>
            <span className="font-mono text-[12px] font-medium text-[var(--color-green)] shrink-0">&#10003;</span>
            <span className="font-mono text-[12px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>{d}</span>
          </div>
        ))}
      </div>
    </>
  );
}

function CLIReferencePage() {
  return (
    <>
      <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>CLI Reference</h1>
      <p className="font-mono text-sm leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        The <code className="text-[var(--color-accent)]">anvil</code> CLI provides commands for running the modernization pipeline.
      </p>

      <h2 className="text-xl font-semibold mt-4" style={{ color: "var(--color-text)" }}>anvil run</h2>
      <p className="font-mono text-sm leading-relaxed mt-2" style={{ color: "var(--color-text-secondary)" }}>
        Run the full modernization pipeline on source files.
      </p>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-code-green)" }}>
{`$ anvil run [flags]

Flags:
  --source string    Source language (default "cobol")
  --target string    Target language (default "java")
  --input string     Input directory or file (required)
  --output string    Output directory (required)
  --config string    Config file path (default "anvil.yaml")
  --json             Output results as JSON
  --verbose          Enable verbose logging
  --dry-run          Parse and analyze without generating code`}
        </pre>
      </div>
    </>
  );
}

function PlaceholderPage({ title, desc }: { title: string; desc: string }) {
  return (
    <>
      <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>{title}</h1>
      <p className="font-mono text-sm leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>{desc}</p>
      <div className="flex gap-3 p-4 mt-4" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <AlertTriangle size={18} className="text-[var(--color-accent)] shrink-0 mt-0.5" />
        <span className="font-mono text-[12px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
          This page is under construction. Check back soon or contribute on GitHub.
        </span>
      </div>
    </>
  );
}
