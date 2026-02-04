"use client";

import { motion } from "framer-motion";
import SectionLabel from "./SectionLabel";

const rows = [
  { name: "AWS CardDemo", files: "61", modules: "29", fns: "546", types: "2,032", generated: "60", time: "318ms" },
  { name: "CobolCraft", files: "311", modules: "206", fns: "423", types: "1,363", generated: "270", time: "462ms" },
  { name: "COBOL Library", files: "126", modules: "28", fns: "54", types: "226", generated: "30", time: "17ms" },
  { name: "COBOL Check", files: "413", modules: "2", fns: "12", types: "21", generated: "7", time: "20ms" },
  { name: "Programming Course", files: "3", modules: "3", fns: "24", types: "82", generated: "5", time: "12ms" },
  { name: "COBOL Unit Test", files: "153", modules: "2", fns: "2", types: "44", generated: "4", time: "30ms" },
  { name: "COBOL is Fun", files: "51", modules: "2", fns: "3", types: "78", generated: "4", time: "4ms" },
];

const cols = [
  { key: "name", label: "Project", w: "w-[160px]" },
  { key: "files", label: "Files", w: "w-[60px]" },
  { key: "modules", label: "Modules", w: "w-[70px]" },
  { key: "fns", label: "Functions", w: "w-[80px]" },
  { key: "types", label: "Data Types", w: "w-[90px]" },
  { key: "generated", label: "Generated", w: "w-[80px]" },
  { key: "time", label: "Time", w: "w-[70px]" },
] as const;

export default function BenchmarkResults() {
  return (
    <section id="benchmarks" className="flex flex-col items-center gap-8 md:gap-12 px-4 md:px-[120px] py-12 md:py-20 transition-colors" style={{ backgroundColor: "var(--color-bg)" }}>
      <SectionLabel>BENCHMARKS</SectionLabel>
      <h2 className="text-2xl md:text-[40px] font-bold text-center" style={{ color: "var(--color-text)" }}>Battle-tested on real mainframe code.</h2>

      <motion.div
        initial={{ opacity: 0, y: 20 }}
        whileInView={{ opacity: 1, y: 0 }}
        viewport={{ once: true }}
        transition={{ duration: 0.5 }}
        className="w-full overflow-x-auto"
      >
        <div className="inline-flex flex-col transition-colors min-w-[610px]"
          style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-card-bg)" }}
        >
          {/* Header */}
          <div className="flex px-4 md:px-5 py-3" style={{ borderBottom: "1px solid var(--color-border)" }}>
            {cols.map((c) => (
              <span key={c.key} className={`${c.w} font-mono text-[10px] font-semibold`} style={{ color: "var(--color-text-secondary)" }}>
                {c.label}
              </span>
            ))}
          </div>
          {/* Rows */}
          {rows.map((r, i) => (
            <div
              key={i}
              className="flex px-4 md:px-5 py-3 transition-colors"
              style={{
                borderBottom: i < rows.length - 1 ? "1px solid var(--color-border)" : undefined,
              }}
              onMouseEnter={(e) => (e.currentTarget.style.backgroundColor = "var(--color-surface)")}
              onMouseLeave={(e) => (e.currentTarget.style.backgroundColor = "")}
            >
              <span className="w-[160px] font-mono text-[11px] font-medium" style={{ color: "var(--color-text)" }}>{r.name}</span>
              <span className="w-[60px] font-mono text-[11px]" style={{ color: "var(--color-text-secondary)" }}>{r.files}</span>
              <span className="w-[70px] font-mono text-[11px]" style={{ color: "var(--color-text-secondary)" }}>{r.modules}</span>
              <span className="w-[80px] font-mono text-[11px]" style={{ color: "var(--color-text-secondary)" }}>{r.fns}</span>
              <span className="w-[90px] font-mono text-[11px]" style={{ color: "var(--color-text-secondary)" }}>{r.types}</span>
              <span className="w-[80px] font-mono text-[11px]" style={{ color: "var(--color-text-secondary)" }}>{r.generated}</span>
              <span className="w-[70px] font-mono text-[11px] font-medium text-[var(--color-table-green)]">{r.time}</span>
            </div>
          ))}
        </div>
      </motion.div>

      <p className="font-mono text-xs md:text-sm font-semibold text-center" style={{ color: "var(--color-text)" }}>
        1,118 files &rarr; 380 generated artifacts in under 1 second. Zero cloud required.
      </p>
    </section>
  );
}
