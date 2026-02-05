"use client";

import { motion } from "framer-motion";
import { ChevronRight, ChevronDown, RefreshCw } from "lucide-react";
import SectionLabel from "./SectionLabel";

const agents = [
  { num: "01", name: "Cartographer", role: "Parse & Map", desc: "Reads legacy source and builds a semantic graph of modules, functions, data types, call graphs, and I/O contracts. No LLM needed." },
  { num: "02", name: "Specular", role: "Understand & Document", desc: "AI agent analyzes the semantic graph to extract business rules, validation logic, and domain patterns into a human-readable, debuggable intermediate representation." },
  { num: "03", name: "Architect", role: "Generate Modern Code", desc: "Transforms the enriched IR into your target stack (TypeScript, Python, Go, …). Target plugins emit a deterministic build/run manifest so orchestration stays target-agnostic." },
  { num: "04", name: "Judge", role: "Verify Equivalence", desc: "Validates behavior with regression gates (record/replay, DB diff, compilation) and produces a proof pack you can hand to enterprise reviewers. Failed? Architect retries automatically.", highlight: true },
];

export default function HowItWorks() {
  return (
    <section id="how-it-works" className="flex flex-col items-center gap-8 md:gap-12 px-4 md:px-[120px] py-12 md:py-20 transition-colors" style={{ backgroundColor: "var(--color-bg)" }}>
      <div className="w-10 h-0.5 bg-[var(--color-accent)]" />
      <SectionLabel>HOW IT WORKS</SectionLabel>
      <h2 className="text-2xl md:text-[40px] font-bold text-center" style={{ color: "var(--color-text)" }}>A Pipeline of Specialized AI Agents</h2>
      <p className="font-mono text-[13px] md:text-sm text-center max-w-[700px] leading-relaxed" style={{ color: "var(--color-text-dim)" }}>
        Each agent has one job and does it well. The pipeline runs sequentially,
        with automatic retry loops between Architect and Judge until quality thresholds are met.
      </p>
      <div className="flex flex-col md:flex-row items-stretch w-full">
        {agents.map((a, i) => (
          <div key={i} className="flex flex-col md:flex-row items-center flex-1">
            <motion.div initial={{ opacity: 0, y: 20 }} whileInView={{ opacity: 1, y: 0 }} viewport={{ once: true }} transition={{ delay: i * 0.15, duration: 0.4 }}
              className="flex flex-col gap-3 p-5 md:p-6 w-full transition-colors"
              style={{ backgroundColor: "var(--color-card-bg)", border: `1px solid ${a.highlight ? "var(--color-accent-dim)" : "var(--color-border-light)"}` }}
            >
              <span className="text-2xl md:text-[32px] font-bold text-[var(--color-accent)]">{a.num}</span>
              <span className="text-base md:text-lg font-semibold" style={{ color: "var(--color-text)" }}>{a.name}</span>
              <span className="font-mono text-[11px] font-semibold tracking-[2px] text-[var(--color-accent)]">{a.role.toUpperCase()}</span>
              <p className="font-mono text-[13px] leading-relaxed" style={{ color: "var(--color-text-dim)" }}>{a.desc}</p>
            </motion.div>
            {i < agents.length - 1 && (
              <>
                <div className="hidden md:flex items-center justify-center w-8 pt-20">
                  <ChevronRight size={20} className="text-[var(--color-accent)]" />
                </div>
                <div className="flex md:hidden items-center justify-center h-6">
                  <ChevronDown size={20} className="text-[var(--color-accent)]" />
                </div>
              </>
            )}
          </div>
        ))}
      </div>
      <div className="flex items-center gap-2 justify-center">
        <RefreshCw size={14} className="text-[var(--color-accent)]" />
        <span className="font-mono text-[13px] text-center" style={{ color: "var(--color-text-dim)" }}>Architect ↔ Judge retry loop runs until quality thresholds are met</span>
      </div>
    </section>
  );
}
