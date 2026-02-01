"use client";

import { useState, useCallback } from "react";
import { Play, Loader2, CheckCircle2 } from "lucide-react";
import SectionLabel from "./SectionLabel";

const pipelineStages = [
  { agent: "Cartographer", action: "Parsing source files..." },
  { agent: "Cartographer", action: "Building semantic graph..." },
  { agent: "Specular", action: "Extracting business rules..." },
  { agent: "Architect", action: "Generating Java Spring Boot..." },
  { agent: "Judge", action: "Verifying semantic equivalence..." },
];

const tabs = [
  {
    label: "Calculator (Simple)",
    file: "calculator.cbl",
    code: `       IDENTIFICATION DIVISION.
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
           DISPLAY "SUM: " WS-RESULT.`,
    output: `@Service
public class Calculator {
    private int wsNum1 = 0;
    private int wsNum2 = 0;
    private long wsResult = 0L;

    public void mainParagraph() {
        addNumbers();
    }

    private void addNumbers() {
        wsResult = wsNum1 + wsNum2;
        System.out.println("SUM: " + wsResult);
    }
}`,
    metrics: [
      { value: "1", label: "module" },
      { value: "2", label: "functions" },
      { value: "3", label: "data types" },
      { value: "2ms", label: "processing", green: true },
    ],
  },
  {
    label: "CardDemo - Sign On",
    file: "cosgn00c.cbl",
    code: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. COSGN00C.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-USERID    PIC X(8).
       01 WS-PASSWORD  PIC X(8).
       PROCEDURE DIVISION.
       0000-MAIN.
           EXEC CICS RECEIVE
              MAP('COSGN0A')
              MAPSET('COSGN00')
           END-EXEC.
           PERFORM 1000-VALIDATE.
           STOP RUN.
       1000-VALIDATE.
           IF WS-USERID = SPACES
              DISPLAY "INVALID USER"
           END-IF.`,
    output: `@RestController
@RequestMapping("/auth")
public class SignOnController {
    private String wsUserid;
    private String wsPassword;

    @PostMapping("/login")
    public ResponseEntity<?> main(
            @RequestBody LoginRequest req) {
        this.wsUserid = req.getUserid();
        this.wsPassword = req.getPassword();
        return validate();
    }

    private ResponseEntity<?> validate() {
        if (wsUserid == null || wsUserid.isBlank()) {
            return ResponseEntity.badRequest()
                .body("INVALID USER");
        }
        return ResponseEntity.ok().build();
    }
}`,
    metrics: [
      { value: "1", label: "module" },
      { value: "2", label: "functions" },
      { value: "4", label: "data types" },
      { value: "3ms", label: "processing", green: true },
    ],
  },
  {
    label: "Full Project (Scale)",
    file: "carddemo/",
    code: `# AWS CardDemo — Enterprise Credit Card System
# 61 COBOL source files
# CICS online + batch programs
# DB2 database integration
# BMS screen maps

  Modules:     29
  Functions:   546
  Data Types:  2,032
  Call Edges:  708
  I/O:         532
  Java Files:  60
  Time:        318ms`,
    output: `✓ Generated 60 Java Spring Boot files

  src/main/java/
  ├── controller/     12 files
  ├── service/        18 files
  ├── model/          15 files
  ├── repository/      8 files
  └── config/          7 files

  src/test/java/
  └── service/        18 test files

  All 546 functions mapped.
  All 2,032 data types converted.
  Judge verification: PASS (score: 0.97)`,
    metrics: [
      { value: "61", label: "files" },
      { value: "546", label: "functions" },
      { value: "2,032", label: "data types" },
      { value: "318ms", label: "processing", green: true },
    ],
  },
];

type RunState = "idle" | "running" | "done";

export default function InteractiveDemo() {
  const [active, setActive] = useState(0);
  const [runState, setRunState] = useState<RunState>("idle");
  const [stageIndex, setStageIndex] = useState(0);
  const [outputLines, setOutputLines] = useState<string[]>([]);

  const tab = tabs[active];

  const handleRun = useCallback(() => {
    if (runState === "running") return;
    setRunState("running");
    setStageIndex(0);
    setOutputLines([]);

    const lines = tab.output.split("\n");
    let stage = 0;
    let line = 0;

    const stageInterval = setInterval(() => {
      stage++;
      if (stage < pipelineStages.length) {
        setStageIndex(stage);
      } else {
        clearInterval(stageInterval);
      }
    }, 500);

    const totalDelay = pipelineStages.length * 500;
    const lineDelay = 40;

    setTimeout(() => {
      const lineInterval = setInterval(() => {
        if (line < lines.length) {
          setOutputLines((prev) => [...prev, lines[line]]);
          line++;
        } else {
          clearInterval(lineInterval);
          setRunState("done");
        }
      }, lineDelay);
    }, totalDelay);
  }, [runState, tab.output]);

  const handleTabChange = (i: number) => {
    setActive(i);
    setRunState("idle");
    setOutputLines([]);
    setStageIndex(0);
  };

  return (
    <section
      id="demo"
      className="flex flex-col items-center gap-12 px-[120px] py-20 transition-colors"
      style={{ backgroundColor: "var(--color-surface-alt)", borderTop: "1px solid var(--color-border)", borderBottom: "1px solid var(--color-border)" }}
    >
      <SectionLabel>INTERACTIVE DEMO</SectionLabel>
      <h2 className="text-[40px] font-bold text-center" style={{ color: "var(--color-text)" }}>See it work. Right now.</h2>

      {/* Tabs */}
      <div className="flex w-full" style={{ borderBottom: "1px solid var(--color-border)" }}>
        {tabs.map((t, i) => (
          <button
            key={i}
            onClick={() => handleTabChange(i)}
            className="font-mono text-xs px-6 py-3 transition-colors"
            style={{
              color: i === active ? "var(--color-text)" : "var(--color-text-secondary)",
              backgroundColor: i === active ? "var(--color-surface)" : "transparent",
              borderBottom: i === active ? "2px solid var(--color-accent)" : "1px solid var(--color-border)",
            }}
          >
            {t.label}
          </button>
        ))}
        <div className="flex-1" style={{ borderBottom: "1px solid var(--color-border)" }} />
      </div>

      {/* Body */}
      <div className="flex w-full gap-0">
        {/* Code pane */}
        <div className="flex-1 p-5 flex flex-col gap-3" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-bg)" }}>
          <div className="flex items-center justify-between">
            <span className="font-mono text-[11px]" style={{ color: "var(--color-text-secondary)" }}>
              {tab.file}
            </span>
            <button
              onClick={handleRun}
              disabled={runState === "running"}
              className="flex items-center gap-2 bg-[var(--color-accent)] text-black font-mono text-[11px] font-semibold px-4 py-1.5 hover:brightness-110 transition disabled:opacity-60"
            >
              {runState === "running" ? <Loader2 size={12} className="animate-spin" /> : runState === "done" ? <CheckCircle2 size={12} /> : <Play size={12} />}
              {runState === "running" ? "Running..." : runState === "done" ? "Complete" : "Run Anvil"}
            </button>
          </div>
          <div className="h-px" style={{ backgroundColor: "var(--color-border)" }} />

          {runState === "idle" ? (
            <pre className="font-mono text-xs leading-[1.7] whitespace-pre overflow-x-auto" style={{ color: "var(--color-text-secondary)" }}>
              {tab.code}
            </pre>
          ) : (
            <div className="flex flex-col gap-3">
              {/* Pipeline stages */}
              <div className="flex flex-col gap-1.5">
                {pipelineStages.map((s, i) => (
                  <div key={i} className="flex items-center gap-2 font-mono text-[11px]">
                    {i < stageIndex ? (
                      <CheckCircle2 size={12} className="text-[var(--color-green)]" />
                    ) : i === stageIndex && runState === "running" ? (
                      <Loader2 size={12} className="text-[var(--color-accent)] animate-spin" />
                    ) : (
                      <span className="w-3 h-3 rounded-full" style={{ border: "1px solid var(--color-border)" }} />
                    )}
                    <span style={{ color: i <= stageIndex ? "var(--color-text)" : "var(--color-text-muted)" }}>
                      [{s.agent}]
                    </span>
                    <span style={{ color: i <= stageIndex ? "var(--color-text-secondary)" : "var(--color-text-muted)" }}>
                      {s.action}
                    </span>
                  </div>
                ))}
              </div>

              {/* Output */}
              {outputLines.length > 0 && (
                <>
                  <div className="h-px" style={{ backgroundColor: "var(--color-border)" }} />
                  <pre className="font-mono text-xs leading-[1.7] whitespace-pre overflow-x-auto" style={{ color: "var(--color-code-green)" }}>
                    {outputLines.join("\n")}
                  </pre>
                </>
              )}
            </div>
          )}
        </div>

        {/* Metrics sidebar */}
        <div className="w-[200px] p-5 flex flex-col gap-4" style={{ border: "1px solid var(--color-border)", borderLeft: "none", backgroundColor: "var(--color-surface)" }}>
          <span className="font-mono text-[10px] font-semibold tracking-[2px] text-[var(--color-accent)]">
            METRICS
          </span>
          {tab.metrics.map((m, i) => (
            <div key={i} className="flex flex-col gap-0.5">
              <span className={`font-mono text-xl font-bold ${m.green ? "text-[var(--color-green)]" : ""}`} style={m.green ? {} : { color: "var(--color-text)" }}>
                {m.value}
              </span>
              <span className="font-mono text-[10px]" style={{ color: "var(--color-text-secondary)" }}>
                {m.label}
              </span>
            </div>
          ))}
        </div>
      </div>
    </section>
  );
}
