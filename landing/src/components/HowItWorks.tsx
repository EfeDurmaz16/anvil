"use client";

import { motion } from "framer-motion";
import { ChevronRight, RefreshCw } from "lucide-react";
import SectionLabel from "./SectionLabel";

const agents = [
  { num: "01", name: "Cartographer", role: "Parse & Map", desc: "Reads COBOL source, builds a semantic graph of modules, functions, data types, call graphs, and I/O contracts. No LLM needed." },
  { num: "02", name: "Specular", role: "Extract Business Rules", desc: "AI agent analyzes the semantic graph to identify and document business rules, validation logic, and domain patterns." },
  { num: "03", name: "Architect", role: "Generate Modern Code", desc: "Transforms the enriched IR into idiomatic Java Spring Boot with proper type mapping, Spring annotations, and clean architecture." },
  { num: "04", name: "Judge", role: "Verify Equivalence", desc: "Validates that generated code preserves the original business semantics. Failed? Architect retries automatically (up to 3x).", highlight: true },
];

export default function HowItWorks() {
  return (
    <section id="how-it-works" className="flex flex-col items-center gap-12 px-[120px] py-20 transition-colors" style={{ backgroundColor: "var(--color-bg)" }}>
      <div className="w-10 h-0.5 bg-[var(--color-accent)]" />
      <SectionLabel>HOW IT WORKS</SectionLabel>
      <h2 className="text-[40px] font-bold text-center" style={{ color: "var(--color-text)" }}>A Pipeline of Specialized AI Agents</h2>
      <p className="font-mono text-sm text-center max-w-[700px] leading-relaxed" style={{ color: "var(--color-text-dim)" }}>
        Each agent has one job and does it well. The pipeline runs sequentially,
        with automatic retry loops between Architect and Judge until quality thresholds are met.
      </p>
      <div className="flex items-start w-full">
        {agents.map((a, i) => (
          <div key={i} className="flex items-start flex-1">
            <motion.div initial={{ opacity: 0, y: 20 }} whileInView={{ opacity: 1, y: 0 }} viewport={{ once: true }} transition={{ delay: i * 0.15, duration: 0.4 }}
              className="flex flex-col gap-3 p-6 flex-1 transition-colors"
              style={{ backgroundColor: "var(--color-card-bg)", border: `1px solid ${a.highlight ? "var(--color-accent-dim)" : "var(--color-border-light)"}` }}
            >
              <span className="text-[32px] font-bold text-[var(--color-accent)]">{a.num}</span>
              <span className="text-lg font-semibold" style={{ color: "var(--color-text)" }}>{a.name}</span>
              <span className="font-mono text-[11px] font-semibold tracking-[2px] text-[var(--color-accent)]">{a.role.toUpperCase()}</span>
              <p className="font-mono text-xs leading-relaxed" style={{ color: "var(--color-text-dim)" }}>{a.desc}</p>
            </motion.div>
            {i < agents.length - 1 && (
              <div className="flex items-center justify-center w-8 pt-20">
                <ChevronRight size={20} className="text-[var(--color-accent)]" />
              </div>
            )}
          </div>
        ))}
      </div>
      <div className="flex items-center gap-2 justify-center">
        <RefreshCw size={14} className="text-[var(--color-accent)]" />
        <span className="font-mono text-xs" style={{ color: "var(--color-text-dim)" }}>Architect ↔ Judge retry loop runs until quality thresholds are met</span>
      </div>
    </section>
  );
}
