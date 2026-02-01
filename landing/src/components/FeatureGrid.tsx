"use client";

import { motion } from "framer-motion";
import SectionLabel from "./SectionLabel";

const capabilities = [
  [
    { title: "Fixed-format (cols 1-72)", desc: "Comment detection, Area A/B parsing" },
    { title: "PERFORM THRU/UNTIL/VARYING", desc: "All loop variants mapped" },
    { title: "EXEC CICS (SEND/RECEIVE)", desc: "Screen I/O detection" },
    { title: "EXEC SQL", desc: "DB I/O classification" },
  ],
  [
    { title: "PIC 9/X/A/V/S clauses", desc: "Full type inference" },
    { title: "COMP/COMP-3/BINARY", desc: "Usage clause mapping" },
    { title: "88-level conditions", desc: "Boolean constant detection" },
    { title: "OCCURS (arrays)", desc: "Array type wrapping" },
  ],
  [
    { title: "COPY/REDEFINES", desc: "Copybook + overlay tracking" },
    { title: "EVALUATE/WHEN", desc: "Complexity metrics" },
    { title: "Sections & paragraphs", desc: "Both detected as functions" },
    { title: "CALL 'external-program'", desc: "Cross-program dependency tracking" },
  ],
];

export default function FeatureGrid() {
  return (
    <section className="flex flex-col items-center gap-12 px-[120px] py-20">
      <SectionLabel>CAPABILITIES</SectionLabel>
      <h2 className="text-[40px] font-bold text-center">What Anvil understands.</h2>

      <div className="flex gap-4 w-full">
        {capabilities.map((col, ci) => (
          <div key={ci} className="flex-1 flex flex-col gap-3">
            {col.map((c, ri) => (
              <motion.div
                key={ri}
                initial={{ opacity: 0, y: 10 }}
                whileInView={{ opacity: 1, y: 0 }}
                viewport={{ once: true }}
                transition={{ delay: (ci * 4 + ri) * 0.04, duration: 0.3 }}
                className="flex flex-col gap-2 p-5 border border-[var(--color-border)] bg-[var(--color-bg)]"
              >
                <span className="text-[13px] font-semibold text-[var(--color-text)]">{c.title}</span>
                <span className="font-mono text-[11px] text-[var(--color-text-secondary)] leading-relaxed">
                  {c.desc}
                </span>
                <span className="font-mono text-[10px] font-medium text-[var(--color-green)]">
                  &#10003; Supported
                </span>
              </motion.div>
            ))}
          </div>
        ))}
      </div>
    </section>
  );
}
