"use client";

import SectionLabel from "./SectionLabel";

const terminalCode = `# Step 1: Install
$ go install github.com/efebarandurmaz/anvil/cmd/anvil@latest

# Step 2: Run (no config needed, works offline)
$ anvil run --source cobol --target java \\
  --input ./your-cobol-src \\
  --output ./generated-java

# Step 3: See what happened
$ anvil run --source cobol --target java \\
  --input ./your-cobol-src \\
  --output ./generated-java \\
  --json | jq .

✓ 380 Java files generated in <1s`;

const yamlCode = `llm:
  provider: anthropic  # or openai, groq, ollama
  api_key: \${ANTHROPIC_API_KEY}
  model: claude-sonnet-4-20250514`;

export default function QuickStart() {
  return (
    <section
      id="quick-start"
      className="flex flex-col items-center gap-12 px-[120px] py-20 transition-colors"
      style={{ backgroundColor: "var(--color-surface-alt)", borderTop: "1px solid var(--color-border-light)", borderBottom: "1px solid var(--color-border-light)" }}
    >
      <SectionLabel>QUICK START</SectionLabel>
      <h2 className="text-[40px] font-bold text-center" style={{ color: "var(--color-text)" }}>Up and running in 60 seconds.</h2>
      <p className="font-mono text-[13px]" style={{ color: "var(--color-text-dim)" }}>
        No config needed. Works offline by default.
      </p>

      {/* Terminal */}
      <div className="w-[700px]" style={{ border: "1px solid var(--color-border-light)", backgroundColor: "var(--color-surface-alt)" }}>
        <div className="flex items-center gap-2 px-4 py-2.5" style={{ backgroundColor: "var(--color-surface-alt)" }}>
          <span className="w-2.5 h-2.5 rounded-full bg-[var(--color-accent)]" />
          <span className="w-2.5 h-2.5 rounded-full bg-[var(--color-accent)]" />
          <span className="w-2.5 h-2.5 rounded-full text-[var(--color-green)]" style={{ backgroundColor: "var(--color-green)" }} />
          <span className="font-mono text-[11px] ml-2" style={{ color: "var(--color-text-dim)" }}>terminal</span>
        </div>
        <pre className="font-mono text-[13px] leading-[1.7] px-4 pb-5 whitespace-pre overflow-x-auto" style={{ color: "var(--color-text-dim)" }}>
          {terminalCode}
        </pre>
      </div>

      <p className="font-mono text-xs" style={{ color: "var(--color-text-secondary)" }}>
        Want AI-powered business rule extraction? Add your LLM provider:
      </p>

      {/* YAML box */}
      <div className="w-[500px] p-4 flex flex-col gap-2" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-bg)" }}>
        <span className="font-mono text-[11px]" style={{ color: "var(--color-text-secondary)" }}># anvil.yaml</span>
        <div className="h-px" style={{ backgroundColor: "var(--color-border)" }} />
        <pre className="font-mono text-xs text-[var(--color-accent)] leading-[1.7] whitespace-pre">{yamlCode}</pre>
      </div>
    </section>
  );
}
