"use client";

import SectionLabel from "./SectionLabel";

const cobol = `       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM1 PIC 9(5) VALUE ZEROS.
       01 WS-NUM2 PIC 9(5) VALUE ZEROS.
       01 WS-RESULT PIC 9(10) VALUE ZEROS.
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM ADD-NUMBERS.
           STOP RUN.
       ADD-NUMBERS.
           ADD WS-NUM1 TO WS-NUM2
              GIVING WS-RESULT.
           DISPLAY "SUM: " WS-RESULT.`;

const typescript = `// Target: TypeScript (example)
export function addNumbers(wsNum1 = 0, wsNum2 = 0) {
  const wsResult = wsNum1 + wsNum2;
  return { wsResult, display: "SUM: " + wsResult };
}`;

const mappings = [
  { from: "PIC 9(5)", to: "number" },
  { from: "PIC 9(10)", to: "number" },
  { from: "PERFORM", to: "function()" },
  { from: "WORKING-STORAGE", to: "module state" },
];

export default function CodeComparison() {
  return (
    <section className="flex flex-col items-center gap-8 md:gap-12 px-4 md:px-[120px] py-12 md:py-20">
      <SectionLabel>CODE COMPARISON</SectionLabel>
      <h2 className="text-2xl md:text-[40px] font-bold text-center">From mainframe to microservice.</h2>

      <div className="flex flex-col md:flex-row w-full gap-0">
        {/* COBOL */}
        <div className="flex-1 border border-[var(--color-border)] bg-[var(--color-bg)] p-4 flex flex-col gap-2">
          <div className="flex items-center gap-2">
            <span className="w-2 h-2 rounded bg-[var(--color-text-secondary)]" />
            <span className="font-mono text-[10px] font-semibold tracking-[1px] text-[var(--color-text-secondary)]">COBOL</span>
          </div>
          <div className="h-px bg-[var(--color-border)]" />
          <pre className="font-mono text-[10px] md:text-xs text-[var(--color-text-secondary)] leading-[1.7] whitespace-pre overflow-x-auto">{cobol}</pre>
        </div>

        {/* TypeScript */}
        <div className="flex-1 border md:border-l-0 border-[var(--color-border)] bg-[var(--color-bg)] p-4 flex flex-col gap-2">
          <div className="flex items-center gap-2">
            <span className="w-2 h-2 rounded bg-[var(--color-green)]" />
            <span className="font-mono text-[10px] font-semibold tracking-[1px] text-[var(--color-green)]">TYPESCRIPT</span>
          </div>
          <div className="h-px bg-[var(--color-border)]" />
          <pre className="font-mono text-[10px] md:text-xs text-[var(--color-green)] leading-[1.7] whitespace-pre overflow-x-auto">{typescript}</pre>
        </div>
      </div>

      {/* Type mappings */}
      <div className="flex flex-wrap gap-2 md:gap-4 justify-center w-full">
        {mappings.map((m, i) => (
          <div
            key={i}
            className="flex items-center gap-2 border border-[var(--color-border)] bg-[var(--color-bg)] px-3 md:px-4 py-2"
          >
            <span className="font-mono text-[10px] md:text-[11px] text-[var(--color-text-secondary)]">{m.from}</span>
            <span className="font-mono text-[10px] md:text-[11px] text-[var(--color-accent)]">&rarr;</span>
            <span className="font-mono text-[10px] md:text-[11px] font-semibold text-[var(--color-green)]">{m.to}</span>
          </div>
        ))}
      </div>
    </section>
  );
}
