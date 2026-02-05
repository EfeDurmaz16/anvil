"use client";

import SectionLabel from "./SectionLabel";

const terminalCode = `# Step 1: Install
$ go install github.com/efebarandurmaz/anvil/cmd/anvil@latest

# Step 2: Modernize (pick your source + target)
$ anvil run --source cobol --target typescript \\
  --input ./your-cobol-src \\
  --output ./generated

$ anvil run --source perl --target python \\
  --input ./your-perl-src \\
  --output ./generated

$ anvil run --source fortran --target go \\
  --input ./your-fortran-src \\
  --output ./generated

# Step 3: Regression gate (fixtures → proof pack)
$ anvil harness run --fixtures ./fixtures.jsonl \\
  --code ./generated \\
  --output ./proof-pack

✓ Proof pack written to ./proof-pack/summary.json`;

const yamlCode = `llm:
  provider: ollama  # or openai, anthropic, groq, deepseek, ...
  base_url: http://localhost:11434/v1
  model: codellama
  api_key: ""  # usually not needed for local endpoints`;

export default function QuickStart() {
  return (
    <section
      id="quick-start"
      className="flex flex-col items-center gap-8 md:gap-12 px-4 md:px-[120px] py-12 md:py-20 transition-colors"
      style={{ backgroundColor: "var(--color-surface-alt)", borderTop: "1px solid var(--color-border-light)", borderBottom: "1px solid var(--color-border-light)" }}
    >
      <SectionLabel>QUICK START</SectionLabel>
      <h2 className="text-2xl md:text-[40px] font-bold text-center" style={{ color: "var(--color-text)" }}>Up and running in 60 seconds.</h2>
      <p className="font-mono text-[13px] md:text-sm" style={{ color: "var(--color-text-dim)" }}>
        No config needed. Works offline by default.
      </p>

      {/* Terminal */}
      <div className="w-full max-w-[700px]" style={{ border: "1px solid var(--color-border-light)", backgroundColor: "var(--color-surface-alt)" }}>
        <div className="flex items-center gap-2 px-4 py-2.5" style={{ backgroundColor: "var(--color-surface-alt)" }}>
          <span className="w-2.5 h-2.5 rounded-full bg-[var(--color-accent)]" />
          <span className="w-2.5 h-2.5 rounded-full bg-[var(--color-accent)]" />
          <span className="w-2.5 h-2.5 rounded-full" style={{ backgroundColor: "var(--color-green)" }} />
          <span className="font-mono text-[11px] ml-2" style={{ color: "var(--color-text-dim)" }}>terminal</span>
        </div>
        <pre className="font-mono text-[11px] md:text-[13px] leading-[1.7] px-4 pb-5 whitespace-pre overflow-x-auto" style={{ color: "var(--color-text-dim)" }}>
          {terminalCode}
        </pre>
      </div>

      <p className="font-mono text-xs md:text-[13px] text-center" style={{ color: "var(--color-text-secondary)" }}>
        Want AI-powered business rule extraction? Add your LLM provider:
      </p>

      {/* YAML box */}
      <div className="w-full max-w-[500px] p-4 flex flex-col gap-2" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-bg)" }}>
        <span className="font-mono text-[11px]" style={{ color: "var(--color-text-secondary)" }}># anvil.yaml</span>
        <div className="h-px" style={{ backgroundColor: "var(--color-border)" }} />
        <pre className="font-mono text-xs text-[var(--color-accent)] leading-[1.7] whitespace-pre overflow-x-auto">{yamlCode}</pre>
      </div>
    </section>
  );
}
