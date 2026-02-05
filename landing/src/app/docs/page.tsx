"use client";

import { useState } from "react";
import { Github, Sun, Moon, Search, ChevronRight, ArrowLeft, ArrowRight, AlertTriangle, Zap, Puzzle, Bot, Workflow, Menu, X, Tag, Bug, Wrench, Plus, Clock, User, CheckCircle2, Circle, Milestone } from "lucide-react";
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
  {
    group: "Updates",
    items: [
      { label: "Blog", slug: "blog" },
      { label: "Changelog", slug: "changelog" },
    ],
  },
  {
    group: "Project",
    items: [
      { label: "Roadmap", slug: "roadmap" },
    ],
  },
];

const pages: Record<string, React.ReactNode> = {
  introduction: <IntroductionPage />,
  installation: <InstallationPage />,
  "quick-start": <QuickStartPage />,
  configuration: <ConfigurationPage />,
  cartographer: <AgentPage agent="Cartographer" num="01" role="Parse & Map" desc="The Cartographer agent reads legacy source files (COBOL, Perl, Fortran) and builds a semantic graph — modules, functions, data types, call graphs, and I/O contracts. No LLM needed. Each source language has a dedicated parser plugin." details={["Parses fixed-format COBOL (columns 1-72) with Area A/B detection", "Extracts IDENTIFICATION, DATA, and PROCEDURE divisions", "Builds call graphs from PERFORM, CALL, and GO TO statements", "Maps PIC clauses to typed IR nodes (PIC 9 → numeric, PIC X → string)", "Resolves COPY/INCLUDE copybook dependencies", "Detects EXEC CICS and EXEC SQL blocks for I/O classification", "Parses Perl packages and scripts (sub declarations, use/require, DBI)", "Parses Fortran 90+ modules (subroutine/function, USE, intent parameters)", "Optionally stores the semantic graph in Neo4j and generates vector embeddings in Qdrant"]} />,
  specular: <AgentPage agent="Specular" num="02" role="Extract Business Rules" desc="The Specular agent uses an LLM to analyze the semantic graph and extract business rules, validation logic, and domain patterns from any supported source language. Prompts are dynamically adapted to the source language of each module." details={["Queries the semantic graph for complex conditional logic", "Uses LLM to identify business rules embedded in EVALUATE/IF chains", "Extracts validation patterns (field-level, cross-field, temporal)", "Documents domain-specific terminology and naming conventions", "Scores complexity per function to prioritize review", "Enriches the IR with business rule annotations", "Works without LLM in template-only mode (rules are skipped)"]} />,
  architect: <AgentPage agent="Architect" num="03" role="Generate Modern Code" desc="The Architect agent transforms the enriched IR into your target stack (TypeScript, Python, Go, …). Target plugins emit an execution manifest so orchestration stays target-agnostic." details={["Generates idiomatic target code with deterministic scaffolding", "Emits an anvil.manifest.json (compile + runner commands)", "Uses an LLM for complex logic translation when available", "Falls back to template-based generation in offline mode", "Preserves structure and naming for traceability", "Supports adding new targets via TargetPlugin only (no extra orchestration config)", "Can generate unit-test stubs and harness runners per target"]} />,
  judge: <AgentPage agent="Judge" num="04" role="Verify Equivalence" desc="The Judge agent validates that generated code preserves the original business semantics. It combines semantic checks with regression gates (record/replay, DB diff, compilation) and produces a proof pack." details={["Parses structured LLM verdicts when enabled (strict JSON)", "Scores equivalence with gate-based rules (compile must pass)", "Integrates with fixture replay and DB diff for behavior checks", "Produces proof packs (diffs, logs, SBOM metadata) for reviewers", "On failure, provides actionable feedback for retry", "Retry loop runs up to 3 times before reporting partial success", "Runs in fully offline mode (LLM optional)"]} />,
  "semantic-graph": <SemanticGraphPage />,
  "plugin-system": <PluginSystemPage />,
  "llm-providers": <LLMProvidersPage />,
  "cli-reference": <CLIReferencePage />,
  "config-file": <ConfigFilePage />,
  "api-reference": <APIReferencePage />,
  contributing: <ContributingPage />,
  blog: <BlogPage />,
  changelog: <ChangelogPage />,
  roadmap: <RoadmapPage />,
};

export default function DocsPage() {
  const { theme, toggle } = useTheme();
  const [activePage, setActivePage] = useState("introduction");
  const [sidebarOpen, setSidebarOpen] = useState(false);

  const allItems = sidebarNav.flatMap((g) => g.items);
  const currentIndex = allItems.findIndex((i) => i.slug === activePage);
  const prev = currentIndex > 0 ? allItems[currentIndex - 1] : null;
  const next = currentIndex < allItems.length - 1 ? allItems[currentIndex + 1] : null;
  const currentGroup = sidebarNav.find((g) => g.items.some((i) => i.slug === activePage));

  return (
    <div className="min-h-screen transition-colors" style={{ backgroundColor: "var(--color-bg)" }}>
      {/* Header */}
      <header
        className="sticky top-0 z-50 flex items-center justify-between px-4 md:px-6 py-3 backdrop-blur-md transition-colors"
        style={{ backgroundColor: "var(--color-header-bg)", borderBottom: "1px solid var(--color-border)" }}
      >
        <div className="flex items-center gap-3 md:gap-4">
          <button onClick={() => setSidebarOpen(!sidebarOpen)} className="p-1 md:hidden" style={{ color: "var(--color-text)" }} aria-label="Toggle sidebar">
            {sidebarOpen ? <X size={18} /> : <Menu size={18} />}
          </button>
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

        <div className="flex items-center gap-2 md:gap-3">
          <div className="hidden md:flex items-center gap-2 px-3 py-1.5" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-surface)" }}>
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

      <div className="flex relative">
        {/* Sidebar - hidden on mobile, toggled via hamburger */}
        {sidebarOpen && (
          <div className="fixed inset-0 z-30 bg-black/50 md:hidden" onClick={() => setSidebarOpen(false)} />
        )}
        <aside className={`${sidebarOpen ? "translate-x-0" : "-translate-x-full"} md:translate-x-0 fixed md:sticky top-[53px] z-40 w-[260px] shrink-0 h-[calc(100vh-53px)] overflow-y-auto p-5 flex flex-col gap-6 transition-transform`} style={{ borderRight: "1px solid var(--color-border)", backgroundColor: "var(--color-surface-alt)" }}>
          {sidebarNav.map((group) => (
            <div key={group.group} className="flex flex-col gap-1">
              <span className="font-mono text-[10px] font-semibold tracking-[2px] mb-1" style={{ color: "var(--color-text-muted)" }}>
                {group.group.toUpperCase()}
              </span>
              {group.items.map((item) => (
                <button
                  key={item.slug}
                  onClick={() => { setActivePage(item.slug); setSidebarOpen(false); }}
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
        <main className="flex-1 max-w-[800px] px-4 md:px-12 py-6 md:py-8 flex flex-col gap-6 md:gap-8 min-w-0">
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
      <p className="font-mono text-[15px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        Anvil is an open-source, multi-agent platform that modernizes legacy codebases into production-ready TypeScript, Python, or Go. It uses a pipeline of four specialized agents — Cartographer, Specular, Architect, and Judge — to parse, understand, transform, and verify your code.
      </p>

      <h2 className="text-xl font-semibold mt-4" style={{ color: "var(--color-text)" }}>What is Anvil?</h2>
      <p className="font-mono text-[15px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        Legacy mainframe systems (COBOL) and legacy server code (Perl/Fortran) still power critical infrastructure in banking and telecom. These systems are reliable but hard to evolve. Anvil bridges this gap by translating legacy logic into modern services and validating behavior with regression gates and proof packs.
      </p>

      {/* Alert */}
      <div className="flex gap-3 p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <AlertTriangle size={18} className="text-[var(--color-accent)] shrink-0 mt-0.5" />
        <div className="flex flex-col gap-1">
          <span className="font-mono text-[12px] font-semibold" style={{ color: "var(--color-text)" }}>LLM is optional</span>
          <span className="font-mono text-[13px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
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
          { icon: Zap, title: "Extensible Plugin System", desc: "Multi-source → multi-target. Generate TypeScript, Python, Go (and more) by implementing SourcePlugin/TargetPlugin — orchestration stays the same." },
        ].map(({ icon: Icon, title, desc }) => (
          <div key={title} className="flex gap-3 p-4" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-card-bg)" }}>
            <Icon size={18} className="text-[var(--color-accent)] shrink-0 mt-0.5" />
            <div className="flex flex-col gap-1">
              <span className="font-mono text-[13px] font-semibold" style={{ color: "var(--color-text)" }}>{title}</span>
              <span className="font-mono text-[13px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>{desc}</span>
            </div>
          </div>
        ))}
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Quick Install</h2>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-code-green)" }}>
{`$ go install github.com/efebarandurmaz/anvil/cmd/anvil@latest
$ anvil run --source cobol --target typescript \\
    --input ./your-cobol-src \\
    --output ./generated`}
        </pre>
      </div>
    </>
  );
}

function InstallationPage() {
  return (
    <>
      <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>Installation</h1>
      <p className="font-mono text-[15px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        Anvil requires a recent Go toolchain (see <code className="text-[var(--color-accent)]">go.mod</code>). It has zero external runtime dependencies for basic operation.
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
      <p className="font-mono text-[15px] leading-relaxed mt-2" style={{ color: "var(--color-text-secondary)" }}>
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
      <p className="font-mono text-[15px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        Get Anvil running in under a minute. No configuration required for basic offline operation.
      </p>

      <h2 className="text-xl font-semibold mt-4" style={{ color: "var(--color-text)" }}>1. Install Anvil</h2>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-code-green)" }}>
{`$ go install github.com/efebarandurmaz/anvil/cmd/anvil@latest`}
        </pre>
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>2. Run on Your Legacy Code</h2>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-code-green)" }}>
{`$ anvil run --source cobol --target typescript \\
    --input ./your-cobol-src \\
    --output ./generated`}
        </pre>
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>3. View Results</h2>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-code-green)" }}>
{`$ anvil run --source cobol --target typescript \\
    --input ./your-cobol-src \\
    --output ./generated \\
    --json | jq .`}
        </pre>
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>4. Enable AI Features (Optional)</h2>
      <p className="font-mono text-[15px] leading-relaxed mt-2" style={{ color: "var(--color-text-secondary)" }}>
        Create an <code className="text-[var(--color-accent)]">anvil.yaml</code> file to enable LLM-powered business rule extraction:
      </p>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-accent)" }}>
{`llm:
  provider: ollama
  base_url: http://localhost:11434/v1
  model: codellama
  api_key: ""`}
        </pre>
      </div>
    </>
  );
}

function ConfigurationPage() {
  return (
    <>
      <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>Configuration</h1>
      <p className="font-mono text-[15px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        Anvil is configured through <code className="text-[var(--color-accent)]">anvil.yaml</code>. All options have sensible defaults.
      </p>

      <h2 className="text-xl font-semibold mt-4" style={{ color: "var(--color-text)" }}>Full Configuration Example</h2>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-accent)" }}>
{`# anvil.yaml
llm:
  # provider: anthropic, openai, groq, huggingface, ollama, together, deepseek, custom, none
  provider: none
  model: ""
  api_key: ""
  base_url: ""
  temperature: 0.2
  max_tokens: 4096

graph:
  uri: bolt://localhost:7687
  username: neo4j
  password: \${NEO4J_PASSWORD}

vector:
  host: localhost
  port: 6334
  collection: anvil

temporal:
  host: localhost:7233
  namespace: default
  task_queue: anvil-tasks

log:
  level: info
  format: json`}
        </pre>
      </div>

      <div className="flex gap-3 p-4 mt-4" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <AlertTriangle size={18} className="text-[var(--color-accent)] shrink-0 mt-0.5" />
        <div className="flex flex-col gap-1">
          <span className="font-mono text-[12px] font-semibold" style={{ color: "var(--color-text)" }}>Target selection</span>
          <span className="font-mono text-[13px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
            Target language is selected via CLI/workflow input (<span className="text-[var(--color-accent)]">--target typescript|python|golang|java</span>).
            Target plugins emit an execution manifest so the harness/orchestrator stays target-agnostic.
          </span>
        </div>
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
      <p className="font-mono text-[15px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>{desc}</p>

      <h2 className="text-xl font-semibold mt-4" style={{ color: "var(--color-text)" }}>What it does</h2>
      <div className="flex flex-col gap-2 mt-2">
        {details.map((d, i) => (
          <div key={i} className="flex gap-3 p-3" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-card-bg)" }}>
            <span className="font-mono text-[12px] font-medium text-[var(--color-green)] shrink-0">&#10003;</span>
            <span className="font-mono text-[13px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>{d}</span>
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
      <p className="font-mono text-[15px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        The <code className="text-[var(--color-accent)]">anvil</code> CLI provides commands for running the modernization pipeline.
      </p>

      <h2 className="text-xl font-semibold mt-4" style={{ color: "var(--color-text)" }}>anvil run</h2>
      <p className="font-mono text-[15px] leading-relaxed mt-2" style={{ color: "var(--color-text-secondary)" }}>
        Run the full modernization pipeline on source files.
      </p>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-code-green)" }}>
{`$ anvil run [flags]

Flags:
  --source string    Source language: cobol, perl, fortran (default "cobol")
  --target string    Target language: typescript, python, golang, java (default "typescript")
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

function SemanticGraphPage() {
  return (
    <>
      <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>Semantic Graph (IR)</h1>
      <p className="font-mono text-[15px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        The Semantic Graph is Anvil{"'"}s central intermediate representation — a typed, queryable structure that captures the full understanding of a legacy codebase.
      </p>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Core Structures</h2>
      <div className="flex flex-col gap-2 mt-2">
        {[
          { name: "Module", desc: "A top-level unit (COBOL program, Perl package, Fortran module). Contains functions, data types, and metadata including the source language." },
          { name: "Function", desc: "A callable unit (COBOL paragraph/section, Perl sub, Fortran subroutine/function). Includes name, parameters, return type, body source, and complexity score." },
          { name: "DataType", desc: "A typed field or structure. Supports String, Integer, Decimal, Boolean, Array, and Struct kinds. Includes size, precision, and nested fields." },
          { name: "BusinessRule", desc: "An LLM-extracted annotation linking a rule description to a source reference (module.function). Only populated when an LLM provider is configured." },
          { name: "CallEdge", desc: "A directed edge in the call graph from caller to callee. Captures PERFORM, CALL, use, and subroutine call relationships." },
          { name: "IOOperation", desc: "A file, database, or network I/O operation. Classified as FileRead, FileWrite, DBQuery, DBUpdate, NetworkCall, or ScreenIO." },
        ].map((item) => (
          <div key={item.name} className="flex gap-3 p-3" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-card-bg)" }}>
            <span className="font-mono text-[12px] font-semibold w-28 shrink-0" style={{ color: "var(--color-accent)" }}>{item.name}</span>
            <span className="font-mono text-[13px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>{item.desc}</span>
          </div>
        ))}
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Storage Backends</h2>
      <p className="font-mono text-[15px] leading-relaxed mt-2" style={{ color: "var(--color-text-secondary)" }}>
        The semantic graph can optionally be persisted to external stores for querying and visualization:
      </p>
      <div className="flex flex-col gap-2 mt-3">
        {[
          { name: "Neo4j", desc: "Graph database for storing modules, functions, and call edges. Enables Cypher queries for cross-module analysis and dependency tracing." },
          { name: "Qdrant", desc: "Vector database for semantic search. Stores embeddings of function bodies and business rules for similarity-based retrieval." },
        ].map((item) => (
          <div key={item.name} className="flex gap-3 p-3" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-card-bg)" }}>
            <span className="font-mono text-[12px] font-semibold w-24 shrink-0" style={{ color: "var(--color-accent)" }}>{item.name}</span>
            <span className="font-mono text-[13px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>{item.desc}</span>
          </div>
        ))}
      </div>
    </>
  );
}

function PluginSystemPage() {
  return (
    <>
      <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>Plugin System</h1>
      <p className="font-mono text-[15px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        Anvil{"'"}s plugin architecture decouples source parsing from target generation. Add new languages without changing the orchestration layer.
      </p>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Source Plugins</h2>
      <p className="font-mono text-[15px] leading-relaxed mt-2" style={{ color: "var(--color-text-secondary)" }}>
        Source plugins parse legacy files into the semantic graph (IR). Each plugin implements the <code className="text-[var(--color-accent)]">SourcePlugin</code> interface:
      </p>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-code-green)" }}>
{`type SourcePlugin interface {
    Language() string
    Parse(ctx context.Context, files []SourceFile) (*ir.SemanticGraph, error)
    ResolveDependencies(ctx context.Context, graph *ir.SemanticGraph) error
}`}
        </pre>
      </div>
      <div className="flex flex-col gap-2 mt-4">
        {[
          { name: "COBOL", desc: "Parses fixed-format COBOL (columns 1-72). Handles IDENTIFICATION, DATA, and PROCEDURE divisions, COPY/INCLUDE copybooks, EXEC SQL, and EXEC CICS blocks." },
          { name: "Perl", desc: "Parses Perl packages and scripts. Extracts sub declarations, use/require dependencies, DBI database calls, and OO patterns (bless, method calls)." },
          { name: "Fortran", desc: "Parses Fortran 90+ modules. Extracts subroutine/function definitions, USE dependencies, intent(in/out/inout) parameters, and DO loop structures." },
        ].map((item) => (
          <div key={item.name} className="flex gap-3 p-3" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-card-bg)" }}>
            <span className="font-mono text-[12px] font-semibold w-20 shrink-0" style={{ color: "var(--color-accent)" }}>{item.name}</span>
            <span className="font-mono text-[13px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>{item.desc}</span>
          </div>
        ))}
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Target Plugins</h2>
      <p className="font-mono text-[15px] leading-relaxed mt-2" style={{ color: "var(--color-text-secondary)" }}>
        Target plugins transform the enriched IR into modern code. Each plugin implements the <code className="text-[var(--color-accent)]">TargetPlugin</code> interface and emits an <code className="text-[var(--color-accent)]">anvil.manifest.json</code> for the harness:
      </p>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-code-green)" }}>
{`type TargetPlugin interface {
    Language() string
    Generate(ctx context.Context, graph *ir.SemanticGraph, provider llm.Provider) ([]GeneratedFile, error)
    Scaffold(ctx context.Context, graph *ir.SemanticGraph) ([]GeneratedFile, error)
}`}
        </pre>
      </div>
      <div className="flex flex-col gap-2 mt-4">
        {[
          { name: "TypeScript", desc: "Generates idiomatic TypeScript with ES modules. Uses bigint for financial precision. Emits npm project structure with package.json and tsconfig." },
          { name: "Python", desc: "Generates Python 3.10+ with type hints. Uses the decimal module for financial precision. Emits project with pyproject.toml." },
          { name: "Go", desc: "Generates idiomatic Go with shopspring/decimal for financial precision. Emits go.mod and a cmd/ entrypoint." },
          { name: "Java", desc: "Generates Java Spring Boot services with BigDecimal for financial precision. Emits Maven pom.xml, JPA entities, and Spring Data repositories." },
        ].map((item) => (
          <div key={item.name} className="flex gap-3 p-3" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-card-bg)" }}>
            <span className="font-mono text-[12px] font-semibold w-24 shrink-0" style={{ color: "var(--color-accent)" }}>{item.name}</span>
            <span className="font-mono text-[13px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>{item.desc}</span>
          </div>
        ))}
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Registry</h2>
      <p className="font-mono text-[15px] leading-relaxed mt-2" style={{ color: "var(--color-text-secondary)" }}>
        Plugins are registered at startup via the thread-safe <code className="text-[var(--color-accent)]">Registry</code>. The CLI resolves <code className="text-[var(--color-accent)]">--source</code> and <code className="text-[var(--color-accent)]">--target</code> flags to the corresponding plugins automatically.
      </p>
    </>
  );
}

function LLMProvidersPage() {
  return (
    <>
      <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>LLM Providers</h1>
      <p className="font-mono text-[15px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        Anvil supports multiple LLM providers through a unified interface. Configure your provider in <code className="text-[var(--color-accent)]">anvil.yaml</code> or run fully offline with template-only mode.
      </p>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Supported Providers</h2>
      <div className="flex flex-col gap-2 mt-2">
        {[
          { name: "Anthropic", models: "Claude Sonnet, Claude Opus", completion: true, embedding: false, note: "Use OpenAI-compatible endpoint for embeddings" },
          { name: "OpenAI", models: "GPT-4, GPT-4o, o1", completion: true, embedding: true, note: "Full completion + embedding support" },
          { name: "Groq", models: "Llama, Mixtral", completion: true, embedding: false, note: "Fast inference via OpenAI-compatible API" },
          { name: "Ollama", models: "CodeLlama, Llama, etc.", completion: true, embedding: true, note: "Fully local, no API key required" },
          { name: "vLLM", models: "Any supported model", completion: true, embedding: true, note: "Self-hosted via OpenAI-compatible API" },
        ].map((p) => (
          <div key={p.name} className="flex flex-col gap-1 p-3" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-card-bg)" }}>
            <div className="flex items-center gap-3">
              <span className="font-mono text-[12px] font-semibold w-20 shrink-0" style={{ color: "var(--color-accent)" }}>{p.name}</span>
              <span className="font-mono text-[11px]" style={{ color: "var(--color-text-secondary)" }}>{p.models}</span>
            </div>
            <div className="flex items-center gap-3 ml-[calc(5rem+12px)]">
              <span className="font-mono text-[10px]" style={{ color: p.completion ? "var(--color-green)" : "var(--color-text-muted)" }}>{p.completion ? "✓" : "✗"} Completion</span>
              <span className="font-mono text-[10px]" style={{ color: p.embedding ? "var(--color-green)" : "var(--color-text-muted)" }}>{p.embedding ? "✓" : "✗"} Embedding</span>
            </div>
            <span className="font-mono text-[11px] ml-[calc(5rem+12px)]" style={{ color: "var(--color-text-muted)" }}>{p.note}</span>
          </div>
        ))}
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Configuration</h2>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-accent)" }}>
{`# anvil.yaml
llm:
  provider: anthropic    # anthropic, openai, groq, ollama, none
  model: claude-sonnet-4-20250514
  api_key: "\${ANVIL_LLM_API_KEY}"
  base_url: ""           # custom endpoint (for vLLM, Ollama, etc.)
  temperature: 0.2
  max_tokens: 4096`}
        </pre>
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Offline Mode</h2>
      <p className="font-mono text-[15px] leading-relaxed mt-2" style={{ color: "var(--color-text-secondary)" }}>
        Set <code className="text-[var(--color-accent)]">provider: none</code> to run Anvil fully offline. The Cartographer and Architect agents work without an LLM using template-based generation. The Specular agent (business rule extraction) and Judge agent (LLM-based verification) will skip their LLM steps gracefully.
      </p>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Rate Limiting</h2>
      <p className="font-mono text-[15px] leading-relaxed mt-2" style={{ color: "var(--color-text-secondary)" }}>
        Anvil includes built-in rate limiting with exponential backoff and retry logic. Configure via <code className="text-[var(--color-accent)]">llm.rate_limit</code> settings or use the defaults (suitable for most providers).
      </p>
    </>
  );
}

function ConfigFilePage() {
  return (
    <>
      <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>Config File</h1>
      <p className="font-mono text-[15px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        Anvil is configured through <code className="text-[var(--color-accent)]">anvil.yaml</code>. All settings have sensible defaults — you can run Anvil with zero configuration for offline template-based generation.
      </p>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Full Reference</h2>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-accent)" }}>
{`# anvil.yaml — Full configuration reference

# ─── LLM Provider ───────────────────────────
llm:
  provider: none          # anthropic | openai | groq | ollama | none
  model: ""               # e.g. claude-sonnet-4-20250514, gpt-4o, codellama
  api_key: ""             # or use ANVIL_LLM_API_KEY env var
  base_url: ""            # custom endpoint (vLLM, Ollama, etc.)
  temperature: 0.2        # 0.0-1.0, lower = more deterministic
  max_tokens: 4096        # max output tokens per LLM call

# ─── Graph Storage (optional) ───────────────
graph:
  uri: bolt://localhost:7687
  username: neo4j
  password: \${NEO4J_PASSWORD}   # env var substitution supported

# ─── Vector Storage (optional) ──────────────
vector:
  host: localhost
  port: 6334
  collection: anvil

# ─── Temporal Orchestration (optional) ──────
temporal:
  host: localhost:7233
  namespace: default
  task_queue: anvil-tasks

# ─── Logging ────────────────────────────────
log:
  level: info             # debug | info | warn | error
  format: json            # json | text`}
        </pre>
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Environment Variables</h2>
      <p className="font-mono text-[15px] leading-relaxed mt-2" style={{ color: "var(--color-text-secondary)" }}>
        All config values can be overridden via environment variables with the <code className="text-[var(--color-accent)]">ANVIL_</code> prefix:
      </p>
      <div className="flex flex-col gap-2 mt-3">
        {[
          { env: "ANVIL_LLM_API_KEY", desc: "LLM provider API key" },
          { env: "ANVIL_LLM_PROVIDER", desc: "LLM provider name (anthropic, openai, etc.)" },
          { env: "ANVIL_LLM_MODEL", desc: "Model name override" },
          { env: "ANVIL_LLM_BASE_URL", desc: "Custom LLM endpoint URL" },
          { env: "ANVIL_GRAPH_URI", desc: "Neo4j connection URI" },
          { env: "ANVIL_GRAPH_PASSWORD", desc: "Neo4j password" },
          { env: "ANVIL_VECTOR_HOST", desc: "Qdrant host" },
          { env: "ANVIL_TEMPORAL_HOST", desc: "Temporal server address" },
        ].map((item) => (
          <div key={item.env} className="flex gap-3 p-3" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-card-bg)" }}>
            <code className="font-mono text-[12px] font-semibold shrink-0" style={{ color: "var(--color-accent)" }}>{item.env}</code>
            <span className="font-mono text-[12px]" style={{ color: "var(--color-text-secondary)" }}>{item.desc}</span>
          </div>
        ))}
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Minimal Config</h2>
      <p className="font-mono text-[15px] leading-relaxed mt-2" style={{ color: "var(--color-text-secondary)" }}>
        For offline template-based generation, no config file is needed at all. Just run:
      </p>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-code-green)" }}>
{`$ anvil run --source cobol --target typescript --input ./src --output ./out`}
        </pre>
      </div>
    </>
  );
}

function APIReferencePage() {
  return (
    <>
      <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>API Reference</h1>
      <p className="font-mono text-[15px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        Anvil exposes a Go API for programmatic access. Import the packages directly to embed Anvil in your own tools and workflows.
      </p>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Core Packages</h2>
      <div className="flex flex-col gap-2 mt-2">
        {[
          { pkg: "ir", path: "github.com/efebarandurmaz/anvil/internal/ir", desc: "Intermediate Representation types — SemanticGraph, Module, Function, DataType, BusinessRule, CallEdge, IOOperation." },
          { pkg: "llm", path: "github.com/efebarandurmaz/anvil/internal/llm", desc: "LLM provider interface and implementations. Provider, Prompt, Response types. Supports Anthropic, OpenAI, and OpenAI-compatible endpoints." },
          { pkg: "plugins", path: "github.com/efebarandurmaz/anvil/internal/plugins", desc: "SourcePlugin and TargetPlugin interfaces. Registry for registering and resolving plugins by language name." },
          { pkg: "agents", path: "github.com/efebarandurmaz/anvil/internal/agents", desc: "Agent interface and implementations for Cartographer, Specular, Architect, and Judge. Each agent operates on the SemanticGraph." },
          { pkg: "config", path: "github.com/efebarandurmaz/anvil/internal/config", desc: "Configuration loading via Viper. Supports YAML files and ANVIL_ environment variable overrides. Includes Validate() for pre-flight checks." },
          { pkg: "harness", path: "github.com/efebarandurmaz/anvil/internal/harness", desc: "Record/replay harness for behavioral verification. Runs fixtures against generated code and produces proof packs." },
          { pkg: "temporal", path: "github.com/efebarandurmaz/anvil/internal/temporal", desc: "Temporal workflow and activity definitions. ModernizationWorkflow orchestrates the 4-agent pipeline with retry logic." },
        ].map((item) => (
          <div key={item.pkg} className="flex flex-col gap-1 p-3" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-card-bg)" }}>
            <div className="flex items-center gap-2">
              <span className="font-mono text-[12px] font-semibold" style={{ color: "var(--color-accent)" }}>{item.pkg}</span>
            </div>
            <code className="font-mono text-[10px]" style={{ color: "var(--color-text-muted)" }}>{item.path}</code>
            <span className="font-mono text-[13px] leading-relaxed mt-1" style={{ color: "var(--color-text-secondary)" }}>{item.desc}</span>
          </div>
        ))}
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Plugin Interface</h2>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-code-green)" }}>
{`// Register a custom source plugin
registry := plugins.NewRegistry()
registry.RegisterSource(myparser.New())

// Register a custom target plugin
registry.RegisterTarget(mygenerator.New())

// Resolve by name
src, _ := registry.GetSource("cobol")
tgt, _ := registry.GetTarget("typescript")`}
        </pre>
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Running a Pipeline Programmatically</h2>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-code-green)" }}>
{`// 1. Parse source files
graph, _ := sourcePlugin.Parse(ctx, files)

// 2. Extract business rules (optional, needs LLM)
specular.EnrichGraph(ctx, graph, llmProvider)

// 3. Generate target code
generated, _ := targetPlugin.Generate(ctx, graph, llmProvider)

// 4. Verify equivalence
result := judge.Verify(ctx, graph, generated)`}
        </pre>
      </div>
    </>
  );
}

function ContributingPage() {
  return (
    <>
      <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>Contributing</h1>
      <p className="font-mono text-[15px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        Anvil is open source under the MIT license. We welcome contributions of all kinds.
      </p>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Getting Started</h2>
      <div className="p-4 mt-2" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <pre className="font-mono text-[13px] leading-[1.7]" style={{ color: "var(--color-code-green)" }}>
{`$ git clone https://github.com/EfeDurmaz16/anvil.git
$ cd anvil
$ go mod download
$ go build ./...
$ go test ./...`}
        </pre>
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Contribution Areas</h2>
      <div className="flex flex-col gap-2 mt-2">
        {[
          { area: "Source Plugins", desc: "Add parsers for new legacy languages (PL/I, RPG, Pascal, etc.). Implement the SourcePlugin interface and register in the worker.", priority: "High" },
          { area: "Target Plugins", desc: "Add code generators for new target platforms (Kotlin, Rust, C#, etc.). Implement the TargetPlugin interface with manifest + runner.", priority: "High" },
          { area: "Parser Improvements", desc: "Improve existing parsers — better COBOL copybook resolution, Perl regex handling, Fortran 2003+ features.", priority: "Medium" },
          { area: "Test Coverage", desc: "Add unit tests, integration tests, and E2E pipeline tests. Especially for edge cases in source parsing.", priority: "Medium" },
          { area: "Documentation", desc: "Improve docs, add examples, write tutorials. Help make Anvil accessible to enterprise teams.", priority: "Medium" },
          { area: "Bug Reports", desc: "Found a bug? Open an issue with reproduction steps. Parsing bugs are especially valuable — include the source file if possible.", priority: "Always Welcome" },
        ].map((item) => (
          <div key={item.area} className="flex flex-col gap-1 p-3" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-card-bg)" }}>
            <div className="flex items-center gap-3">
              <span className="font-mono text-[12px] font-semibold" style={{ color: "var(--color-accent)" }}>{item.area}</span>
              <span className="font-mono text-[10px] px-2 py-0.5" style={{ color: "var(--color-green)", border: "1px solid var(--color-green)" }}>{item.priority}</span>
            </div>
            <span className="font-mono text-[13px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>{item.desc}</span>
          </div>
        ))}
      </div>

      <h2 className="text-xl font-semibold mt-6" style={{ color: "var(--color-text)" }}>Pull Request Process</h2>
      <div className="flex flex-col gap-2 mt-2">
        {[
          "Fork the repository and create a feature branch",
          "Write tests for your changes (go test ./...)",
          "Ensure go vet ./... and go build ./... pass",
          "Submit a PR with a clear description of the change",
          "Respond to review feedback",
        ].map((step, i) => (
          <div key={i} className="flex gap-3 p-3" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-card-bg)" }}>
            <span className="font-mono text-[12px] font-semibold shrink-0" style={{ color: "var(--color-accent)" }}>{i + 1}.</span>
            <span className="font-mono text-[12px]" style={{ color: "var(--color-text-secondary)" }}>{step}</span>
          </div>
        ))}
      </div>
    </>
  );
}

function BlogPage() {
  const [activePost, setActivePost] = useState<string | null>(null);

  const posts = [
    {
      slug: "multi-language-launch",
      title: "Anvil 0.2: From COBOL-Only to a 3×4 Language Matrix",
      excerpt: "Anvil now supports COBOL, Perl, and Fortran as source languages with TypeScript, Python, Go, and Java as targets — 12 migration paths in one platform.",
      date: "2026-02-05",
      readTime: "5 min",
      author: "Efe Baran Durmaz",
      tags: ["release", "multi-language", "enterprise"],
      content: (
        <>
          <p>When we first released Anvil, it was a COBOL-to-Java migration tool. Useful, but narrow. Today we{"'"}re shipping v0.2 with support for <strong>3 source languages</strong> and <strong>4 target languages</strong> — a 12-path migration matrix.</p>
          <h3 className="text-base font-semibold mt-4" style={{ color: "var(--color-text)" }}>Why Multi-Language Matters</h3>
          <p>Enterprise legacy isn{"'"}t just COBOL. Telecom companies run millions of lines of Perl. Scientific computing relies on Fortran. Financial institutions have all three. A modernization platform that only handles one language forces enterprises to cobble together multiple tools.</p>
          <h3 className="text-base font-semibold mt-4" style={{ color: "var(--color-text)" }}>How It Works</h3>
          <p>Anvil{"'"}s secret is the <strong>Semantic Graph (IR)</strong>. Every source language parser produces the same intermediate representation. Target plugins consume this IR without knowing what language produced it. Adding a new source language is just implementing <code className="text-[var(--color-accent)]">SourcePlugin</code> — three methods.</p>
          <h3 className="text-base font-semibold mt-4" style={{ color: "var(--color-text)" }}>Dynamic LLM Prompts</h3>
          <p>In v0.1, our LLM prompts were hardcoded to COBOL. In v0.2, prompts dynamically inject the source language from each module{"'"}s metadata. The Specular agent now generates language-appropriate rule extraction prompts.</p>
          <h3 className="text-base font-semibold mt-4" style={{ color: "var(--color-text)" }}>Enterprise Hardening</h3>
          <p>Beyond multi-language, v0.2 includes: LLM rate limiting with exponential backoff, SQL injection fixes, CI/CD pipeline, configuration validation, and proper error handling throughout.</p>
        </>
      ),
    },
    {
      slug: "why-agents",
      title: "Why We Built Anvil as a Multi-Agent System",
      excerpt: "Most code migration tools are monolithic transpilers. Anvil uses four specialized agents in a pipeline. Here's why.",
      date: "2026-01-25",
      readTime: "7 min",
      author: "Efe Baran Durmaz",
      tags: ["architecture", "agents", "design"],
      content: (
        <>
          <p>The first version of Anvil was a single Go function: read COBOL, call an LLM, write Java. It worked on toy examples and fell apart on anything real.</p>
          <h3 className="text-base font-semibold mt-4" style={{ color: "var(--color-text)" }}>The Problem with Monolithic Transpilers</h3>
          <p>A single-pass transpiler tries to do everything at once. When something goes wrong, you can{"'"}t tell which step failed. Was the parse wrong? Did the LLM hallucinate?</p>
          <h3 className="text-base font-semibold mt-4" style={{ color: "var(--color-text)" }}>Four Agents, Four Concerns</h3>
          <p><strong>Cartographer</strong> — Parse source files into a semantic graph. No LLM needed. <strong>Specular</strong> — Extract business rules via LLM. <strong>Architect</strong> — Generate target code from enriched IR. <strong>Judge</strong> — Verify semantic equivalence and produce proof packs.</p>
          <h3 className="text-base font-semibold mt-4" style={{ color: "var(--color-text)" }}>Why This Matters</h3>
          <p>When a migration fails, you know exactly where. Each agent can be tested, debugged, and improved independently. In production, these run as Temporal activities with retry logic, observability, and durability.</p>
        </>
      ),
    },
    {
      slug: "semantic-graph-deep-dive",
      title: "The Semantic Graph: How Anvil Understands Legacy Code",
      excerpt: "A look at the intermediate representation that makes language-agnostic migration possible.",
      date: "2026-01-15",
      readTime: "6 min",
      author: "Efe Baran Durmaz",
      tags: ["technical", "ir", "architecture"],
      content: (
        <>
          <p>Every migration tool needs an intermediate representation. Most use an AST. Anvil uses a <strong>Semantic Graph</strong> that captures meaning, not just structure.</p>
          <h3 className="text-base font-semibold mt-4" style={{ color: "var(--color-text)" }}>Beyond the AST</h3>
          <p>An AST tells you {'"'}this is a PERFORM statement{'"'}. A semantic graph tells you {'"'}this function calls these others, operates on these data types, and implements these business rules{'"'}.</p>
          <h3 className="text-base font-semibold mt-4" style={{ color: "var(--color-text)" }}>Language Agnosticism</h3>
          <p>A COBOL paragraph, a Perl subroutine, and a Fortran function all become <code className="text-[var(--color-accent)]">Function</code> nodes. The source language is metadata; the graph structure is identical.</p>
          <h3 className="text-base font-semibold mt-4" style={{ color: "var(--color-text)" }}>Storage and Querying</h3>
          <p>The graph can live in memory or be persisted to Neo4j for cross-module querying. Combined with Qdrant vector embeddings, you can do semantic search across the entire codebase.</p>
        </>
      ),
    },
  ];

  const post = activePost ? posts.find((p) => p.slug === activePost) : null;

  return (
    <>
      <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>Blog</h1>
      <p className="font-mono text-[14px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>Engineering updates, architecture decisions, and migration insights.</p>

      {!activePost ? (
        <div className="flex flex-col gap-4 mt-2">
          {posts.map((p) => (
            <button key={p.slug} onClick={() => setActivePost(p.slug)} className="text-left flex flex-col gap-2.5 p-4 transition-colors" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-card-bg)" }}
              onMouseEnter={(e) => (e.currentTarget.style.backgroundColor = "var(--color-surface)")}
              onMouseLeave={(e) => (e.currentTarget.style.backgroundColor = "var(--color-card-bg)")}
            >
              <div className="flex flex-wrap gap-2">
                {p.tags.map((tag) => (
                  <span key={tag} className="font-mono text-[10px] px-2 py-0.5" style={{ color: "var(--color-accent)", border: "1px solid var(--color-accent-dim)" }}>{tag}</span>
                ))}
              </div>
              <h2 className="text-base font-semibold" style={{ color: "var(--color-text)" }}>{p.title}</h2>
              <p className="font-mono text-[13px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>{p.excerpt}</p>
              <div className="flex items-center gap-4">
                <span className="flex items-center gap-1.5 font-mono text-[11px]" style={{ color: "var(--color-text-muted)" }}>
                  <Clock size={12} /> {p.date}
                </span>
                <span className="font-mono text-[11px]" style={{ color: "var(--color-text-muted)" }}>{p.readTime}</span>
                <span className="flex items-center gap-1.5 font-mono text-[11px]" style={{ color: "var(--color-text-muted)" }}>
                  <User size={12} /> {p.author}
                </span>
              </div>
            </button>
          ))}
        </div>
      ) : post ? (
        <div className="flex flex-col gap-4">
          <button onClick={() => setActivePost(null)} className="flex items-center gap-2 font-mono text-[12px] self-start" style={{ color: "var(--color-accent)" }}>
            <ArrowLeft size={12} /> Back to all posts
          </button>
          <div className="flex flex-wrap gap-2">
            {post.tags.map((tag) => (
              <span key={tag} className="font-mono text-[10px] px-2 py-0.5" style={{ color: "var(--color-accent)", border: "1px solid var(--color-accent-dim)" }}>{tag}</span>
            ))}
          </div>
          <h2 className="text-xl font-bold" style={{ color: "var(--color-text)" }}>{post.title}</h2>
          <div className="flex items-center gap-4">
            <span className="flex items-center gap-1.5 font-mono text-[11px]" style={{ color: "var(--color-text-muted)" }}><Clock size={12} /> {post.date}</span>
            <span className="font-mono text-[11px]" style={{ color: "var(--color-text-muted)" }}>{post.readTime}</span>
            <span className="flex items-center gap-1.5 font-mono text-[11px]" style={{ color: "var(--color-text-muted)" }}><User size={12} /> {post.author}</span>
          </div>
          <div className="h-px" style={{ backgroundColor: "var(--color-border)" }} />
          <div className="flex flex-col gap-3 font-mono text-[14px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
            {post.content}
          </div>
        </div>
      ) : null}
    </>
  );
}

function ChangelogPage() {
  const changelogEntries = [
    {
      version: "0.2.0",
      date: "2026-02-05",
      title: "Multi-Language & Enterprise Hardening",
      highlights: "3×4 language matrix, Java target with manifest/runner, enterprise-grade reliability",
      changes: [
        { type: "feature" as const, text: "Perl source plugin — parses packages, subs, DBI calls, and OO patterns" },
        { type: "feature" as const, text: "Fortran source plugin — parses modules, subroutines, functions, and USE dependencies" },
        { type: "feature" as const, text: "Java Spring Boot target with anvil.manifest.json and fixture runner" },
        { type: "feature" as const, text: "Dynamic LLM prompts — source language injected per-module instead of hardcoded COBOL" },
        { type: "feature" as const, text: "E2E pipeline test (COBOL → TypeScript template mode)" },
        { type: "feature" as const, text: "Config validation with Validate() pre-flight warnings" },
        { type: "improvement" as const, text: "LLM rate limiting with exponential backoff and retry logic" },
        { type: "improvement" as const, text: "Worker plugin registration — all source/target plugins wired at startup" },
        { type: "improvement" as const, text: "Agent result JSON marshaling with proper error handling" },
        { type: "fix" as const, text: "SQL injection vulnerability in harness DB diff — now uses parameterized queries" },
        { type: "fix" as const, text: "go.mod version aligned to Go 1.24 across Dockerfile and CI" },
        { type: "improvement" as const, text: "CI pipeline — go vet + build + test on push/PR to main" },
      ],
    },
    {
      version: "0.1.0",
      date: "2026-01-20",
      title: "Initial Release",
      highlights: "COBOL source parser, 4-agent pipeline, TypeScript/Python/Go targets, Temporal orchestration",
      changes: [
        { type: "feature" as const, text: "4-agent pipeline: Cartographer → Specular → Architect → Judge" },
        { type: "feature" as const, text: "COBOL source plugin with DATA/PROCEDURE division parsing" },
        { type: "feature" as const, text: "TypeScript target plugin with bigint financial precision" },
        { type: "feature" as const, text: "Python target plugin with decimal module support" },
        { type: "feature" as const, text: "Go target plugin with shopspring/decimal" },
        { type: "feature" as const, text: "Temporal workflow orchestration with retry loop" },
        { type: "feature" as const, text: "Neo4j graph storage for semantic graphs" },
        { type: "feature" as const, text: "Qdrant vector storage for embeddings" },
        { type: "feature" as const, text: "Record/replay harness with proof packs" },
        { type: "feature" as const, text: "LLM provider interface — Anthropic, OpenAI, and OpenAI-compatible" },
        { type: "feature" as const, text: "CLI with cobra — anvil run and anvil harness commands" },
        { type: "feature" as const, text: "Template-only offline mode (no LLM required)" },
      ],
    },
  ];

  const typeStyles: Record<string, { label: string; color: string }> = {
    feature: { label: "New", color: "var(--color-green)" },
    fix: { label: "Fix", color: "#ef4444" },
    improvement: { label: "Improved", color: "var(--color-accent)" },
    breaking: { label: "Breaking", color: "#a855f7" },
  };

  return (
    <>
      <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>Changelog</h1>
      <p className="font-mono text-[14px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>All notable changes to Anvil.</p>

      {changelogEntries.map((entry) => (
        <div key={entry.version} className="flex flex-col gap-3 mt-4">
          <div className="flex items-center gap-3">
            <Tag size={16} style={{ color: "var(--color-accent)" }} />
            <span className="font-mono text-lg font-bold" style={{ color: "var(--color-text)" }}>v{entry.version}</span>
            <span className="font-mono text-[11px]" style={{ color: "var(--color-text-muted)" }}>{entry.date}</span>
          </div>
          <h2 className="text-base font-semibold" style={{ color: "var(--color-text)" }}>{entry.title}</h2>
          <p className="font-mono text-[13px]" style={{ color: "var(--color-text-secondary)" }}>{entry.highlights}</p>

          <div className="flex flex-col gap-1.5">
            {entry.changes.map((change, i) => {
              const style = typeStyles[change.type];
              return (
                <div key={i} className="flex items-start gap-2.5 p-2.5" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-card-bg)" }}>
                  <span className="font-mono text-[10px] font-semibold px-1.5 py-0.5 shrink-0 mt-px" style={{ color: style.color, border: `1px solid ${style.color}` }}>
                    {style.label}
                  </span>
                  <span className="font-mono text-[13px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>{change.text}</span>
                </div>
              );
            })}
          </div>

          <div className="h-px mt-2" style={{ backgroundColor: "var(--color-border)" }} />
        </div>
      ))}
    </>
  );
}

function RoadmapPage() {
  const phases: Array<{ phase: string; quarter: string; theme: string; items: Array<{ title: string; desc: string; status: "done" | "in-progress" | "planned" }> }> = [
    {
      phase: "Phase 1", quarter: "Q1 2026", theme: "Foundation",
      items: [
        { title: "4-Agent Pipeline", desc: "Cartographer → Specular → Architect → Judge with retry loop", status: "done" as const },
        { title: "COBOL Source Plugin", desc: "Fixed-format COBOL parser with DATA/PROCEDURE division, copybooks, EXEC SQL/CICS", status: "done" as const },
        { title: "TypeScript Target Plugin", desc: "Idiomatic TS generation with bigint financial precision and npm project scaffold", status: "done" as const },
        { title: "Python Target Plugin", desc: "Python 3.10+ with type hints and decimal module for financial precision", status: "done" as const },
        { title: "Go Target Plugin", desc: "Idiomatic Go with shopspring/decimal and go.mod scaffold", status: "done" as const },
        { title: "Temporal Orchestration", desc: "Production-grade workflow engine with retry logic and observability", status: "done" as const },
        { title: "Record/Replay Harness", desc: "Fixture-based behavioral verification with proof packs", status: "done" as const },
        { title: "LLM Provider Interface", desc: "Anthropic, OpenAI, and OpenAI-compatible (Ollama, vLLM, Groq)", status: "done" as const },
      ],
    },
    {
      phase: "Phase 2", quarter: "Q1 2026", theme: "Multi-Language & Enterprise",
      items: [
        { title: "Perl Source Plugin", desc: "Perl package/sub parser with DBI, OO patterns, and use/require resolution", status: "done" as const },
        { title: "Fortran Source Plugin", desc: "Fortran 90+ module parser with subroutines, functions, and intent parameters", status: "done" as const },
        { title: "Java Spring Boot Target", desc: "Java target with BigDecimal, JPA entities, Spring Data repos, and manifest/runner", status: "done" as const },
        { title: "Dynamic LLM Prompts", desc: "Source language injected per-module instead of hardcoded COBOL references", status: "done" as const },
        { title: "LLM Rate Limiting", desc: "Exponential backoff with configurable rate limits per provider", status: "done" as const },
        { title: "SQL Injection Fix", desc: "Parameterized queries in harness DB diff to prevent injection attacks", status: "done" as const },
        { title: "CI/CD Pipeline", desc: "GitHub Actions with go vet + build + test on push/PR", status: "done" as const },
        { title: "Config Validation", desc: "Pre-flight validation warnings for missing or invalid configuration", status: "done" as const },
      ],
    },
    {
      phase: "Phase 3", quarter: "Q2 2026", theme: "Deep Parsing & Quality",
      items: [
        { title: "Tree-sitter Integration", desc: "Optional tree-sitter parsing for deeper AST analysis (behind build tag)", status: "in-progress" as const },
        { title: "COBOL Copybook Resolution", desc: "Full transitive COPY/INCLUDE resolution with library paths", status: "in-progress" as const },
        { title: "Perl Regex & Dispatch", desc: "Better regex extraction and method dispatch resolution for complex Perl", status: "planned" as const },
        { title: "Fortran 2003+ Features", desc: "Derived types, type-bound procedures, and abstract interfaces", status: "planned" as const },
        { title: "Incremental Migration", desc: "Migrate individual modules without re-processing the entire codebase", status: "planned" as const },
        { title: "Test Generation", desc: "Auto-generate unit tests for migrated functions based on fixture data", status: "planned" as const },
      ],
    },
    {
      phase: "Phase 4", quarter: "Q3 2026", theme: "New Languages & Scale",
      items: [
        { title: "PL/I Source Plugin", desc: "PL/I parser for IBM mainframe workloads", status: "planned" as const },
        { title: "RPG Source Plugin", desc: "RPG/400 and RPG IV parser for IBM i (AS/400) modernization", status: "planned" as const },
        { title: "Kotlin Target Plugin", desc: "Kotlin generation with coroutines and Spring Boot integration", status: "planned" as const },
        { title: "C# Target Plugin", desc: ".NET 8 generation with ASP.NET Core and Entity Framework", status: "planned" as const },
        { title: "Web Dashboard", desc: "Real-time monitoring for migration pipelines", status: "planned" as const },
        { title: "Parallel Agent Execution", desc: "Run Cartographer on multiple files simultaneously", status: "planned" as const },
      ],
    },
    {
      phase: "Phase 5", quarter: "Q4 2026", theme: "Enterprise Platform",
      items: [
        { title: "Multi-Tenant API", desc: "REST/gRPC API for embedding Anvil in CI/CD pipelines", status: "planned" as const },
        { title: "Migration Analytics", desc: "Complexity scoring, effort estimation, and progress dashboards", status: "planned" as const },
        { title: "Custom Rule Engine", desc: "User-defined business rule templates for domain-specific transformations", status: "planned" as const },
        { title: "Audit Trail", desc: "Complete provenance tracking for every transformation decision", status: "planned" as const },
        { title: "On-Prem Packaging", desc: "Helm charts and air-gapped deployment for regulated environments", status: "planned" as const },
      ],
    },
  ];

  const statusStyles = {
    done: { icon: CheckCircle2, label: "Done", color: "var(--color-green)" },
    "in-progress": { icon: Clock, label: "In Progress", color: "var(--color-accent)" },
    planned: { icon: Circle, label: "Planned", color: "var(--color-text-muted)" },
  };

  return (
    <>
      <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>Roadmap</h1>
      <p className="font-mono text-[14px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
        Where Anvil is headed. Phases 1-2 are shipped. Phase 3 is in progress.
      </p>

      {/* Progress summary */}
      <div className="flex gap-3 flex-wrap mt-2">
        {(["done", "in-progress", "planned"] as const).map((status) => {
          const config = statusStyles[status];
          const Icon = config.icon;
          const count = phases.flatMap((p) => p.items).filter((i: any) => i.status === status).length;
          return (
            <div key={status} className="flex items-center gap-2 px-3 py-2" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-card-bg)" }}>
              <Icon size={14} style={{ color: config.color }} />
              <span className="font-mono text-[11px] font-semibold" style={{ color: config.color }}>{count}</span>
              <span className="font-mono text-[11px]" style={{ color: "var(--color-text-secondary)" }}>{config.label}</span>
            </div>
          );
        })}
      </div>

      {/* Timeline */}
      {phases.map((phase, phaseIdx) => (
        <div key={phase.phase} className="flex flex-col gap-3 mt-4">
          <div className="flex items-center gap-3">
            <Milestone size={18} style={{ color: "var(--color-accent)" }} />
            <span className="font-mono text-base font-bold" style={{ color: "var(--color-text)" }}>{phase.phase}</span>
            <span className="font-mono text-[11px] px-2 py-0.5" style={{ color: "var(--color-accent)", border: "1px solid var(--color-accent-dim)" }}>{phase.quarter}</span>
          </div>
          <span className="font-mono text-[13px] font-semibold" style={{ color: "var(--color-text-secondary)" }}>{phase.theme}</span>

          <div className="flex flex-col gap-1.5 ml-2 pl-4" style={{ borderLeft: "2px solid var(--color-border)" }}>
            {phase.items.map((item) => {
              const config = statusStyles[item.status];
              const Icon = config.icon;
              return (
                <div key={item.title} className="flex items-start gap-3 p-3" style={{ backgroundColor: "var(--color-card-bg)" }}>
                  <Icon size={14} className="shrink-0 mt-0.5" style={{ color: config.color }} />
                  <div className="flex flex-col gap-0.5">
                    <span className="font-mono text-[13px] font-semibold" style={{ color: "var(--color-text)" }}>{item.title}</span>
                    <span className="font-mono text-[12px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>{item.desc}</span>
                  </div>
                </div>
              );
            })}
          </div>

          {phaseIdx < phases.length - 1 && <div className="h-px mt-2" style={{ backgroundColor: "var(--color-border)" }} />}
        </div>
      ))}
    </>
  );
}

function PlaceholderPage({ title, desc }: { title: string; desc: string }) {
  return (
    <>
      <h1 className="text-[32px] font-bold" style={{ color: "var(--color-text)" }}>{title}</h1>
      <p className="font-mono text-[15px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>{desc}</p>
      <div className="flex gap-3 p-4 mt-4" style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-border)" }}>
        <AlertTriangle size={18} className="text-[var(--color-accent)] shrink-0 mt-0.5" />
        <span className="font-mono text-[13px] leading-relaxed" style={{ color: "var(--color-text-secondary)" }}>
          This page is under construction. Check back soon or contribute on GitHub.
        </span>
      </div>
    </>
  );
}
