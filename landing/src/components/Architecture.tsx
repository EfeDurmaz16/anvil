"use client";

import { Brain, Plug, Workflow, Database, Search } from "lucide-react";
import SectionLabel from "./SectionLabel";

const features = [
  {
    icon: Brain,
    title: "Pluggable LLM",
    desc: "Works with Claude, GPT-4, Groq, Ollama, vLLM, or no LLM at all. Template-only mode runs fully offline.",
  },
  {
    icon: Plug,
    title: "Source/Target Plugins",
    desc: "COBOL \u2192 Java today. The plugin interface supports any source language and any target. RPG, PL/I, Fortran \u2014 bring your own parser.",
  },
  {
    icon: Workflow,
    title: "Temporal Orchestration",
    desc: "Production-grade workflow engine with retry logic, observability, and distributed execution. Not a script.",
  },
];

export default function Architecture() {
  return (
    <section
      id="architecture"
      className="flex flex-col items-center gap-12 px-[120px] py-20 bg-[var(--color-surface-alt)] border-y border-[var(--color-border-light)]"
    >
      <SectionLabel>ARCHITECTURE</SectionLabel>
      <h2 className="text-[40px] font-bold text-center">Designed for extensibility.</h2>

      {/* Diagram */}
      <div className="flex flex-col items-center gap-4 w-[800px] border border-[var(--color-border)] bg-[var(--color-bg)] p-8">
        <div className="border border-[var(--color-border)] bg-[var(--color-surface)] px-6 py-2.5">
          <span className="font-mono text-xs font-semibold text-[var(--color-text)]">CLI / API</span>
        </div>
        <div className="w-px h-5 bg-[var(--color-border)]" />
        <div className="border border-[var(--color-accent)] bg-[#f9731622] px-6 py-2.5">
          <span className="font-mono text-xs font-semibold text-[var(--color-accent)]">Temporal Workflow</span>
        </div>
        <div className="w-px h-5 bg-[var(--color-border)]" />
        <div className="flex gap-3 justify-center">
          {["Cartographer", "Specular", "Architect", "Judge"].map((a) => (
            <div key={a} className="border border-[var(--color-border)] bg-[var(--color-surface)] px-4 py-2">
              <span className="font-mono text-[11px] text-[var(--color-text-secondary)]">{a}</span>
            </div>
          ))}
        </div>
        <div className="w-px h-5 bg-[var(--color-border)]" />
        <div className="border border-[var(--color-border)] bg-[var(--color-surface)] px-6 py-2.5">
          <span className="font-mono text-xs font-semibold text-[var(--color-text)]">Semantic Graph (IR)</span>
        </div>
        <div className="w-px h-5 bg-[var(--color-border)]" />
        <div className="flex gap-3 justify-center">
          {[
            { icon: Database, label: "Neo4j" },
            { icon: Search, label: "Qdrant" },
            { icon: Brain, label: "LLM Provider" },
          ].map(({ icon: Icon, label }) => (
            <div key={label} className="flex items-center gap-1.5 border border-[var(--color-border)] bg-[var(--color-surface)] px-4 py-2">
              <Icon size={14} className="text-[var(--color-blue)]" />
              <span className="font-mono text-[11px] text-[var(--color-text-secondary)]">{label}</span>
            </div>
          ))}
        </div>
      </div>

      {/* Feature cards */}
      <div className="flex gap-4 w-full">
        {features.map(({ icon: Icon, title, desc }) => (
          <div
            key={title}
            className="flex-1 flex flex-col gap-3 p-6 border border-[var(--color-border)] bg-[var(--color-bg)]"
          >
            <Icon size={20} className="text-[var(--color-accent)]" />
            <span className="text-base font-semibold text-[var(--color-text)]">{title}</span>
            <p className="font-mono text-xs text-[var(--color-text-secondary)] leading-relaxed">{desc}</p>
          </div>
        ))}
      </div>
    </section>
  );
}
