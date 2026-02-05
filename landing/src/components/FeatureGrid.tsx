"use client";

import { motion } from "framer-motion";
import SectionLabel from "./SectionLabel";

const cobolCapabilities = [
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

const otherLanguages = [
  {
    lang: "Perl",
    color: "#39457E",
    features: [
      "package declarations",
      "sub parsing with brace tracking",
      "use/require dependencies",
      "$/@/% variable detection",
      "file I/O (open/close)",
      "DBI database calls",
      "method calls ($obj->method)",
    ],
  },
  {
    lang: "Fortran",
    color: "#734F96",
    features: [
      "program/module/subroutine/function",
      "USE module dependencies",
      "7 type declarations (integer, real, etc.)",
      "intent(in/out/inout) parameters",
      "CALL statements & call graph",
      "OPEN/READ/WRITE I/O",
      "CONTAINS internal subprograms",
    ],
  },
];

export default function FeatureGrid() {
  return (
    <section className="flex flex-col items-center gap-8 md:gap-12 px-4 md:px-[120px] py-12 md:py-20">
      <SectionLabel>CAPABILITIES</SectionLabel>
      <h2 className="text-2xl md:text-[40px] font-bold text-center">What Anvil understands.</h2>

      {/* COBOL Capabilities */}
      <div className="w-full">
        <h3 className="text-lg md:text-xl font-semibold mb-4 text-[var(--color-text)] flex items-center gap-2">
          <span className="w-3 h-3 rounded-full" style={{ backgroundColor: "#005CA5" }} />
          COBOL
        </h3>
        <div className="flex flex-col md:flex-row gap-3 md:gap-4 w-full">
          {cobolCapabilities.map((col, ci) => (
            <div key={ci} className="flex-1 flex flex-col gap-3">
              {col.map((c, ri) => (
                <motion.div
                  key={ri}
                  initial={{ opacity: 0, y: 10 }}
                  whileInView={{ opacity: 1, y: 0 }}
                  viewport={{ once: true }}
                  transition={{ delay: (ci * 4 + ri) * 0.04, duration: 0.3 }}
                  className="flex flex-col gap-2 p-4 md:p-5 border border-[var(--color-border)] bg-[var(--color-bg)]"
                >
                  <span className="text-[12px] md:text-[13px] font-semibold text-[var(--color-text)]">{c.title}</span>
                  <span className="font-mono text-[11px] md:text-[13px] text-[var(--color-text-secondary)] leading-relaxed">
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
      </div>

      {/* Perl & Fortran */}
      <div className="flex flex-col md:flex-row gap-4 w-full mt-4">
        {otherLanguages.map((lang, i) => (
          <motion.div
            key={lang.lang}
            initial={{ opacity: 0, y: 10 }}
            whileInView={{ opacity: 1, y: 0 }}
            viewport={{ once: true }}
            transition={{ delay: i * 0.1, duration: 0.3 }}
            className="flex-1 p-5 border border-[var(--color-border)] bg-[var(--color-bg)]"
          >
            <h3 className="text-lg font-semibold mb-3 text-[var(--color-text)] flex items-center gap-2">
              <span className="w-3 h-3 rounded-full" style={{ backgroundColor: lang.color }} />
              {lang.lang}
            </h3>
            <ul className="space-y-2">
              {lang.features.map((f, fi) => (
                <li key={fi} className="flex items-start gap-2">
                  <span className="font-mono text-[10px] font-medium text-[var(--color-green)] mt-0.5">&#10003;</span>
                  <span className="font-mono text-[13px] text-[var(--color-text-secondary)]">{f}</span>
                </li>
              ))}
            </ul>
          </motion.div>
        ))}
      </div>
    </section>
  );
}
