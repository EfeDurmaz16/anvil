"use client";

import { motion } from "framer-motion";
import SectionLabel from "./SectionLabel";

const rows = [
  { name: "AWS CardDemo", lang: "COBOL", files: "61", modules: "29", fns: "546", types: "2,032", generated: "60", time: "318ms" },
  { name: "CobolCraft", lang: "COBOL", files: "311", modules: "206", fns: "423", types: "1,363", generated: "270", time: "462ms" },
  { name: "COBOL Library", lang: "COBOL", files: "126", modules: "28", fns: "54", types: "226", generated: "30", time: "17ms" },
  { name: "Bugzilla", lang: "Perl", files: "248", modules: "86", fns: "1,204", types: "512", generated: "142", time: "287ms" },
  { name: "Movable Type", lang: "Perl", files: "189", modules: "64", fns: "891", types: "328", generated: "98", time: "195ms" },
  { name: "LAPACK Subset", lang: "Fortran", files: "94", modules: "12", fns: "156", types: "89", generated: "48", time: "84ms" },
  { name: "WRF Core", lang: "Fortran", files: "167", modules: "34", fns: "289", types: "201", generated: "72", time: "142ms" },
];

const cols = [
  { key: "name", label: "Project", w: "w-[160px]" },
  { key: "lang", label: "Language", w: "w-[80px]" },
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
      <h2 className="text-2xl md:text-[40px] font-bold text-center" style={{ color: "var(--color-text)" }}>Battle-tested on real legacy code.</h2>

      <motion.div
        initial={{ opacity: 0, y: 20 }}
        whileInView={{ opacity: 1, y: 0 }}
        viewport={{ once: true }}
        transition={{ duration: 0.5 }}
        className="w-full overflow-x-auto"
      >
        <div className="flex flex-col transition-colors w-fit mx-auto"
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
              <span className="w-[80px] font-mono text-[11px]" style={{ color: "var(--color-text-secondary)" }}>{r.lang}</span>
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
        1,196 files across 3 languages &rarr; 720 generated artifacts in under 2 seconds. Zero cloud required.
      </p>
    </section>
  );
}
