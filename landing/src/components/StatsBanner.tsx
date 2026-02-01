"use client";

import AnimatedCounter from "./AnimatedCounter";

const stats = [
  { value: 1118, label: "source files parsed" },
  { value: 272, label: "modules analyzed" },
  { value: 1064, label: "functions extracted" },
  { value: 3846, label: "data types mapped" },
  { value: 380, label: "Java files generated" },
];

export default function StatsBanner() {
  return (
    <section className="flex flex-col items-center gap-4 px-4 md:px-[120px] py-8 md:py-10 transition-colors"
      style={{ backgroundColor: "var(--color-surface-alt)", borderTop: "1px solid var(--color-border)", borderBottom: "1px solid var(--color-border)" }}
    >
      <div className="flex flex-wrap items-center justify-center gap-6 md:gap-8 w-full">
        {stats.map((s, i) => (
          <div key={i} className="flex items-center gap-6 md:gap-8">
            {i > 0 && <div className="hidden md:block w-px h-10" style={{ backgroundColor: "var(--color-border)" }} />}
            <div className="flex flex-col items-center gap-1">
              <span className="font-mono text-xl md:text-[28px] font-bold" style={{ color: "var(--color-text)" }}>
                <AnimatedCounter target={s.value} />
              </span>
              <span className="font-mono text-[10px] md:text-[11px]" style={{ color: "var(--color-text-secondary)" }}>{s.label}</span>
            </div>
          </div>
        ))}
      </div>
      <p className="font-mono text-[10px] text-center" style={{ color: "var(--color-text-muted)" }}>
        Benchmarked across 7 open-source COBOL projects including AWS CardDemo, CobolCraft, and IBM samples.
      </p>
    </section>
  );
}
