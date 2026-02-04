"use client";

import { useState, useCallback } from "react";
import { Play, Loader2, CheckCircle2 } from "lucide-react";
import SectionLabel from "./SectionLabel";

const pipelineStages = [
  { agent: "Cartographer", action: "Parsing source files..." },
  { agent: "Cartographer", action: "Building semantic graph..." },
  { agent: "Specular", action: "Extracting business rules..." },
  { agent: "Architect", action: "Generating target code..." },
  { agent: "Judge", action: "Verifying semantic equivalence..." },
];

const tabs = [
  {
    label: "COBOL",
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
    output: `// Target: TypeScript (example)
export function addNumbers(wsNum1 = 0, wsNum2 = 0) {
  const wsResult = wsNum1 + wsNum2;
  return { wsResult, display: "SUM: " + wsResult };
}`,
    metrics: [
      { value: "1", label: "module" },
      { value: "2", label: "functions" },
      { value: "3", label: "data types" },
      { value: "2ms", label: "processing", green: true },
    ],
  },
  {
    label: "Perl",
    file: "BugReport.pm",
    code: `package Bugzilla::BugReport;
use strict;
use warnings;

use Bugzilla::DB;
use Bugzilla::User;

our $VERSION = '1.0';

sub new {
    my ($class, $id) = @_;
    my $self = { id => $id };
    return bless $self, $class;
}

sub fetch_bug {
    my ($self) = @_;
    my $dbh = Bugzilla->dbh;
    my $sth = $dbh->prepare(
        "SELECT * FROM bugs WHERE id = ?"
    );
    $sth->execute($self->{id});
    return $sth->fetchrow_hashref;
}

sub update_status {
    my ($self, $status) = @_;
    open(my $fh, '>>', 'audit.log');
    print $fh "Bug $self->{id}: $status\\n";
    close($fh);
}

1;`,
    output: `// Target: TypeScript (example)
export class BugReportService {
  constructor(
    private db: { query: (sql: string, args: unknown[]) => Promise<any> },
    private audit: { log: (line: string) => void },
  ) {}

  async fetchBug(id: number) {
    const rows = await this.db.query("SELECT * FROM bugs WHERE id = ?", [id]);
    return rows?.[0] ?? null;
  }

  updateStatus(id: number, status: string) {
    this.audit.log(\`Bug \${id}: \${status}\`);
  }
}`,
    metrics: [
      { value: "1", label: "module" },
      { value: "3", label: "functions" },
      { value: "4", label: "data types" },
      { value: "2ms", label: "processing", green: true },
    ],
  },
  {
    label: "Fortran",
    file: "matrix_ops.f90",
    code: `module matrix_operations
  implicit none
  integer, parameter :: MAX_SIZE = 100

contains

  subroutine multiply(A, B, C, n)
    integer, intent(in) :: n
    real, intent(in) :: A(n,n), B(n,n)
    real, intent(out) :: C(n,n)
    integer :: i, j, k

    do i = 1, n
      do j = 1, n
        C(i,j) = 0.0
        do k = 1, n
          C(i,j) = C(i,j) + A(i,k)*B(k,j)
        end do
      end do
    end do
  end subroutine multiply

  real function determinant(M, n)
    integer, intent(in) :: n
    real, intent(in) :: M(n,n)
    determinant = M(1,1)*M(2,2) - M(1,2)*M(2,1)
  end function determinant

end module matrix_operations`,
    output: `// Target: TypeScript (example)
export function multiply(a: number[][], b: number[][], n: number) {
  const c: number[][] = Array.from({ length: n }, () => Array(n).fill(0));
  for (let i = 0; i < n; i++) {
    for (let j = 0; j < n; j++) {
      for (let k = 0; k < n; k++) c[i][j] += a[i][k] * b[k][j];
    }
  }
  return c;
}

export function determinant2x2(m: number[][]) {
  return m[0][0] * m[1][1] - m[0][1] * m[1][0];
}`,
    metrics: [
      { value: "1", label: "module" },
      { value: "2", label: "functions" },
      { value: "3", label: "data types" },
      { value: "2ms", label: "processing", green: true },
    ],
  },
  {
    label: "CardDemo (61 files)",
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
  Generated:   60
  Time:        318ms`,
    output: `✓ Generated 60 target files

  src/
  ├── api/            12 handlers
  ├── domain/         18 services
  ├── model/          15 data models
  ├── persistence/     8 repositories
  └── config/          7 runtime configs

  Regression gate: PASS (fixtures + DB diff)
  Proof pack: ./proof-pack/summary.json`,
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
      className="flex flex-col items-center gap-8 md:gap-12 px-4 md:px-[120px] py-12 md:py-20 transition-colors"
      style={{ backgroundColor: "var(--color-surface-alt)", borderTop: "1px solid var(--color-border)", borderBottom: "1px solid var(--color-border)" }}
    >
      <SectionLabel>INTERACTIVE DEMO</SectionLabel>
      <h2 className="text-2xl md:text-[40px] font-bold text-center" style={{ color: "var(--color-text)" }}>See it work. Right now.</h2>

      {/* Tabs */}
      <div className="flex w-full overflow-x-auto" style={{ borderBottom: "1px solid var(--color-border)" }}>
        {tabs.map((t, i) => (
          <button
            key={i}
            onClick={() => handleTabChange(i)}
            className="font-mono text-[11px] md:text-xs px-4 md:px-6 py-3 transition-colors whitespace-nowrap shrink-0"
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
      <div className="flex flex-col md:flex-row w-full gap-0">
        {/* Code pane */}
        <div className="flex-1 p-4 md:p-5 flex flex-col gap-3" style={{ border: "1px solid var(--color-border)", backgroundColor: "var(--color-bg)" }}>
          <div className="flex items-center justify-between">
            <span className="font-mono text-[11px]" style={{ color: "var(--color-text-secondary)" }}>
              {tab.file}
            </span>
            <button
              onClick={handleRun}
              disabled={runState === "running"}
              className="flex items-center gap-2 bg-[var(--color-accent)] text-black font-mono text-[11px] font-semibold px-3 md:px-4 py-1.5 hover:brightness-110 transition disabled:opacity-60"
            >
              {runState === "running" ? <Loader2 size={12} className="animate-spin" /> : runState === "done" ? <CheckCircle2 size={12} /> : <Play size={12} />}
              {runState === "running" ? "Running..." : runState === "done" ? "Complete" : "Run Anvil"}
            </button>
          </div>
          <div className="h-px" style={{ backgroundColor: "var(--color-border)" }} />

          {runState === "idle" ? (
            <pre className="font-mono text-[10px] md:text-xs leading-[1.7] whitespace-pre overflow-x-auto" style={{ color: "var(--color-text-secondary)" }}>
              {tab.code}
            </pre>
          ) : (
            <div className="flex flex-col gap-3">
              <div className="flex flex-col gap-1.5">
                {pipelineStages.map((s, i) => (
                  <div key={i} className="flex items-center gap-2 font-mono text-[10px] md:text-[11px]">
                    {i < stageIndex ? (
                      <CheckCircle2 size={12} className="text-[var(--color-green)] shrink-0" />
                    ) : i === stageIndex && runState === "running" ? (
                      <Loader2 size={12} className="text-[var(--color-accent)] animate-spin shrink-0" />
                    ) : (
                      <span className="w-3 h-3 rounded-full shrink-0" style={{ border: "1px solid var(--color-border)" }} />
                    )}
                    <span style={{ color: i <= stageIndex ? "var(--color-text)" : "var(--color-text-muted)" }}>
                      [{s.agent}]
                    </span>
                    <span className="truncate" style={{ color: i <= stageIndex ? "var(--color-text-secondary)" : "var(--color-text-muted)" }}>
                      {s.action}
                    </span>
                  </div>
                ))}
              </div>

              {outputLines.length > 0 && (
                <>
                  <div className="h-px" style={{ backgroundColor: "var(--color-border)" }} />
                  <pre className="font-mono text-[10px] md:text-xs leading-[1.7] whitespace-pre overflow-x-auto" style={{ color: "var(--color-code-green)" }}>
                    {outputLines.join("\n")}
                  </pre>
                </>
              )}
            </div>
          )}
        </div>

        {/* Metrics sidebar */}
        <div className="flex md:flex-col gap-4 p-4 md:p-5 md:w-[200px] border border-t-0 md:border-t md:border-l-0 border-[var(--color-border)]" style={{ backgroundColor: "var(--color-surface)" }}>
          <span className="font-mono text-[10px] font-semibold tracking-[2px] text-[var(--color-accent)] hidden md:block">
            METRICS
          </span>
          <div className="flex md:flex-col gap-4 md:gap-4 flex-1">
            {tab.metrics.map((m, i) => (
              <div key={i} className="flex flex-col gap-0.5">
                <span className={`font-mono text-lg md:text-xl font-bold ${m.green ? "text-[var(--color-green)]" : ""}`} style={m.green ? {} : { color: "var(--color-text)" }}>
                  {m.value}
                </span>
                <span className="font-mono text-[10px]" style={{ color: "var(--color-text-secondary)" }}>
                  {m.label}
                </span>
              </div>
            ))}
          </div>
        </div>
      </div>
    </section>
  );
}
