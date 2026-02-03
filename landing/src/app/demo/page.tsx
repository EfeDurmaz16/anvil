"use client";

import { useState, useEffect, useRef, useCallback } from "react";
import { motion, AnimatePresence } from "framer-motion";
import { Play, Loader2, CheckCircle2, Terminal, Code2, ArrowLeft, RotateCcw, Sun, Moon } from "lucide-react";
import Link from "next/link";
import { useTheme } from "@/components/ThemeProvider";

/* ═══════════════════════════════════════════════════════
   Example definitions — each is a realistic migration
   ═══════════════════════════════════════════════════════ */

interface Example {
  label: string;
  legacyFile: string;
  modernFile: string;
  legacy: string;
  modern: string;
  logs: { agent: string; msg: string }[];
  summary: string;
  metrics: { value: string; label: string; green?: boolean }[];
}

const examples: Example[] = [
  /* ── 1. Arithmetic ── */
  {
    label: "Calculator",
    legacyFile: "calc.cbl",
    modernFile: "Calc.java",
    legacy: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-AMT    PIC 9(7)V99 VALUE ZEROS.
       01 WS-RATE   PIC 9(3)V99 VALUE ZEROS.
       01 WS-RES    PIC 9(10)V99 VALUE ZEROS.
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           ACCEPT WS-AMT.
           ACCEPT WS-RATE.
           COMPUTE WS-RES = WS-AMT * WS-RATE.
           DISPLAY "RESULT: " WS-RES.
           STOP RUN.`,
    modern: `import java.math.BigDecimal;

@Service
public class Calc {

    public BigDecimal calculate(
            BigDecimal amt,
            BigDecimal rate) {
        return amt.multiply(rate);
    }
}`,
    logs: [
      { agent: "Cartographer", msg: "Parsing COBOL source..." },
      { agent: "Cartographer", msg: "Building abstract syntax tree..." },
      { agent: "Cartographer", msg: "Resolving data divisions..." },
      { agent: "Specular", msg: "Extracting business logic..." },
      { agent: "Specular", msg: "Mapping COMPUTE → arithmetic ops..." },
      { agent: "Architect", msg: "Selecting Java Spring Boot target..." },
      { agent: "Architect", msg: "Generating service class..." },
      { agent: "Architect", msg: "Mapping PIC 9(n)V99 → BigDecimal..." },
      { agent: "Judge", msg: "Verifying semantic equivalence..." },
      { agent: "Judge", msg: "Score: 0.98 — PASS" },
    ],
    summary: "1 module, 1 function, 3 data types",
    metrics: [
      { value: "1", label: "module" },
      { value: "1", label: "function" },
      { value: "3", label: "data types" },
      { value: "2ms", label: "processing", green: true },
    ],
  },

  /* ── 2. CICS Sign-On → REST Auth ── */
  {
    label: "Auth / Sign-On",
    legacyFile: "cosgn00c.cbl",
    modernFile: "SignOnController.java",
    legacy: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. COSGN00C.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-USERID    PIC X(8).
       01 WS-PASSWORD  PIC X(8).
       01 WS-MSG       PIC X(40).
       01 WS-RETCODE   PIC 9(2) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           EXEC CICS RECEIVE
              MAP('COSGN0A')
              MAPSET('COSGN00')
           END-EXEC.
           PERFORM 1000-VALIDATE.
           IF WS-RETCODE = 0
              EXEC CICS XCTL
                 PROGRAM('COMENU')
              END-EXEC
           ELSE
              MOVE "INVALID CREDENTIALS"
                 TO WS-MSG
              EXEC CICS SEND
                 MAP('COSGN0A')
                 MAPSET('COSGN00')
              END-EXEC
           END-IF.
           STOP RUN.
       1000-VALIDATE.
           IF WS-USERID = SPACES
              OR WS-PASSWORD = SPACES
              MOVE 1 TO WS-RETCODE
           END-IF.`,
    modern: `@RestController
@RequestMapping("/api/auth")
public class SignOnController {

    private final AuthService authService;

    public SignOnController(AuthService authService) {
        this.authService = authService;
    }

    @PostMapping("/login")
    public ResponseEntity<?> login(
            @Valid @RequestBody LoginRequest req) {
        if (req.getUserid() == null
                || req.getUserid().isBlank()
                || req.getPassword() == null
                || req.getPassword().isBlank()) {
            return ResponseEntity.badRequest()
                .body(new ErrorResponse(
                    "INVALID CREDENTIALS"));
        }
        var token = authService.authenticate(
            req.getUserid(), req.getPassword());
        return ResponseEntity.ok(
            new AuthResponse(token));
    }
}`,
    logs: [
      { agent: "Cartographer", msg: "Parsing CICS online program..." },
      { agent: "Cartographer", msg: "Resolving BMS map definitions..." },
      { agent: "Cartographer", msg: "Mapping EXEC CICS RECEIVE → input..." },
      { agent: "Specular", msg: "Extracting authentication flow..." },
      { agent: "Specular", msg: "Detecting screen navigation (XCTL)..." },
      { agent: "Specular", msg: "Mapping return codes → HTTP status..." },
      { agent: "Architect", msg: "Generating REST controller..." },
      { agent: "Architect", msg: "Creating LoginRequest DTO..." },
      { agent: "Architect", msg: "Wiring AuthService dependency..." },
      { agent: "Judge", msg: "Verifying auth path equivalence..." },
      { agent: "Judge", msg: "Score: 0.96 — PASS" },
    ],
    summary: "1 module, 2 functions, 4 data types",
    metrics: [
      { value: "1", label: "module" },
      { value: "2", label: "functions" },
      { value: "4", label: "data types" },
      { value: "3ms", label: "processing", green: true },
    ],
  },

  /* ── 3. Batch Report → Scheduled Service ── */
  {
    label: "Batch Report",
    legacyFile: "rptgen.cbl",
    modernFile: "ReportService.java",
    legacy: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. RPTGEN.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANS-FILE ASSIGN TO
              'TRANSACT.DAT'
              ORGANIZATION IS SEQUENTIAL.
           SELECT REPORT-FILE ASSIGN TO
              'DAILYRPT.TXT'
              ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD TRANS-FILE.
       01 TRANS-REC.
          05 TR-ACCT   PIC X(10).
          05 TR-AMT    PIC 9(7)V99.
          05 TR-DATE   PIC X(8).
       FD REPORT-FILE.
       01 RPT-LINE     PIC X(80).
       WORKING-STORAGE SECTION.
       01 WS-TOTAL     PIC 9(10)V99 VALUE 0.
       01 WS-COUNT     PIC 9(5) VALUE 0.
       01 WS-EOF       PIC X VALUE 'N'.
       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT TRANS-FILE
                OUTPUT REPORT-FILE.
           PERFORM 1000-PROCESS
              UNTIL WS-EOF = 'Y'.
           PERFORM 2000-WRITE-SUMMARY.
           CLOSE TRANS-FILE REPORT-FILE.
           STOP RUN.
       1000-PROCESS.
           READ TRANS-FILE
              AT END MOVE 'Y' TO WS-EOF
              NOT AT END
                 ADD TR-AMT TO WS-TOTAL
                 ADD 1 TO WS-COUNT
           END-READ.
       2000-WRITE-SUMMARY.
           STRING "TOTAL: " WS-TOTAL
              " COUNT: " WS-COUNT
              DELIMITED BY SIZE
              INTO RPT-LINE.
           WRITE RPT-LINE.`,
    modern: `@Service
@Slf4j
public class ReportService {

    private final TransactionRepository txRepo;

    @Scheduled(cron = "0 0 2 * * *")
    public void generateDailyReport() {
        var transactions = txRepo
            .findByDateBetween(
                LocalDate.now().minusDays(1),
                LocalDate.now());

        var summary = transactions.stream()
            .collect(Collectors.summarizingDouble(
                tx -> tx.getAmount()
                    .doubleValue()));

        var report = DailyReport.builder()
            .total(BigDecimal.valueOf(
                summary.getSum()))
            .count(summary.getCount())
            .generatedAt(Instant.now())
            .build();

        reportRepository.save(report);
        log.info("Daily report: {} txns, total {}",
            report.getCount(),
            report.getTotal());
    }
}`,
    logs: [
      { agent: "Cartographer", msg: "Parsing batch program..." },
      { agent: "Cartographer", msg: "Resolving FILE-CONTROL entries..." },
      { agent: "Cartographer", msg: "Mapping FD sections → data models..." },
      { agent: "Specular", msg: "Detecting sequential file I/O pattern..." },
      { agent: "Specular", msg: "Extracting accumulation loop..." },
      { agent: "Specular", msg: "Identifying report generation logic..." },
      { agent: "Architect", msg: "Converting to @Scheduled service..." },
      { agent: "Architect", msg: "Replacing file I/O → JPA repository..." },
      { agent: "Architect", msg: "Mapping STRING → Stream collectors..." },
      { agent: "Architect", msg: "Generating DailyReport entity..." },
      { agent: "Judge", msg: "Verifying aggregation equivalence..." },
      { agent: "Judge", msg: "Score: 0.95 — PASS" },
    ],
    summary: "1 module, 3 functions, 5 data types, 2 file I/O",
    metrics: [
      { value: "1", label: "module" },
      { value: "3", label: "functions" },
      { value: "5", label: "data types" },
      { value: "5ms", label: "processing", green: true },
    ],
  },

  /* ── 4. DB2 CRUD → JPA Repository ── */
  {
    label: "DB2 CRUD",
    legacyFile: "custmnt.cbl",
    modernFile: "CustomerController.java",
    legacy: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTMNT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CUST-ID   PIC X(10).
       01 WS-CUST-NAME PIC X(30).
       01 WS-CUST-BAL  PIC 9(9)V99 VALUE 0.
       01 WS-ACTION    PIC X(1).
         88 ADD-CUST   VALUE 'A'.
         88 UPD-CUST   VALUE 'U'.
         88 DEL-CUST   VALUE 'D'.
         88 INQ-CUST   VALUE 'I'.
       01 WS-SQLCODE   PIC S9(9) COMP.
       PROCEDURE DIVISION.
       0000-MAIN.
           EVALUATE TRUE
              WHEN ADD-CUST
                 PERFORM 1000-INSERT
              WHEN UPD-CUST
                 PERFORM 2000-UPDATE
              WHEN DEL-CUST
                 PERFORM 3000-DELETE
              WHEN INQ-CUST
                 PERFORM 4000-SELECT
           END-EVALUATE.
           STOP RUN.
       1000-INSERT.
           EXEC SQL
              INSERT INTO CUSTOMER
              (CUST_ID, CUST_NAME, BALANCE)
              VALUES
              (:WS-CUST-ID, :WS-CUST-NAME,
               :WS-CUST-BAL)
           END-EXEC.
       2000-UPDATE.
           EXEC SQL
              UPDATE CUSTOMER
              SET CUST_NAME = :WS-CUST-NAME,
                  BALANCE = :WS-CUST-BAL
              WHERE CUST_ID = :WS-CUST-ID
           END-EXEC.
       3000-DELETE.
           EXEC SQL
              DELETE FROM CUSTOMER
              WHERE CUST_ID = :WS-CUST-ID
           END-EXEC.
       4000-SELECT.
           EXEC SQL
              SELECT CUST_NAME, BALANCE
              INTO :WS-CUST-NAME, :WS-CUST-BAL
              FROM CUSTOMER
              WHERE CUST_ID = :WS-CUST-ID
           END-EXEC.`,
    modern: `@RestController
@RequestMapping("/api/customers")
public class CustomerController {

    private final CustomerRepository repo;

    @PostMapping
    public ResponseEntity<Customer> create(
            @Valid @RequestBody CustomerDto dto) {
        var customer = Customer.builder()
            .custId(dto.getCustId())
            .name(dto.getName())
            .balance(dto.getBalance())
            .build();
        return ResponseEntity.status(201)
            .body(repo.save(customer));
    }

    @PutMapping("/{id}")
    public ResponseEntity<Customer> update(
            @PathVariable String id,
            @Valid @RequestBody CustomerDto dto) {
        return repo.findById(id)
            .map(c -> {
                c.setName(dto.getName());
                c.setBalance(dto.getBalance());
                return ResponseEntity.ok(
                    repo.save(c));
            })
            .orElse(ResponseEntity.notFound()
                .build());
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(
            @PathVariable String id) {
        repo.deleteById(id);
        return ResponseEntity.noContent().build();
    }

    @GetMapping("/{id}")
    public ResponseEntity<Customer> findById(
            @PathVariable String id) {
        return repo.findById(id)
            .map(ResponseEntity::ok)
            .orElse(ResponseEntity.notFound()
                .build());
    }
}`,
    logs: [
      { agent: "Cartographer", msg: "Parsing embedded SQL program..." },
      { agent: "Cartographer", msg: "Extracting EXEC SQL blocks..." },
      { agent: "Cartographer", msg: "Mapping host variables → params..." },
      { agent: "Specular", msg: "Detecting CRUD operation pattern..." },
      { agent: "Specular", msg: "Mapping EVALUATE → dispatch table..." },
      { agent: "Specular", msg: "Mapping 88-level → enum actions..." },
      { agent: "Architect", msg: "Generating JPA entity..." },
      { agent: "Architect", msg: "Creating Spring Data repository..." },
      { agent: "Architect", msg: "Generating REST controller..." },
      { agent: "Architect", msg: "Mapping SQLCODE → HTTP responses..." },
      { agent: "Judge", msg: "Verifying CRUD equivalence..." },
      { agent: "Judge", msg: "Score: 0.97 — PASS" },
    ],
    summary: "1 module, 4 functions, 3 data types, 4 SQL ops",
    metrics: [
      { value: "1", label: "module" },
      { value: "4", label: "functions" },
      { value: "3", label: "data types" },
      { value: "4ms", label: "processing", green: true },
    ],
  },

  /* ── 5. Full Enterprise Project ── */
  {
    label: "Enterprise (61 files)",
    legacyFile: "carddemo/",
    modernFile: "carddemo-spring/",
    legacy: `# AWS CardDemo — Enterprise Credit Card System
# 61 COBOL source files across 4 subsystems
#
# ┌─────────────────────────────────────────┐
# │  CICS Online Programs      (23 files)   │
# │  Batch Processing          (18 files)   │
# │  Copybooks / Data Defs     (12 files)   │
# │  BMS Screen Maps           ( 8 files)   │
# └─────────────────────────────────────────┘
#
#  Modules:      29
#  Functions:    546
#  Data Types:   2,032
#  Call Edges:   708
#  File I/O:     532
#  EXEC SQL:     184
#  EXEC CICS:    267`,
    modern: `✓ Generated 60 Java Spring Boot files

  src/main/java/
  ├── controller/       12 REST controllers
  ├── service/          18 service classes
  ├── model/            15 JPA entities
  ├── repository/        8 Spring Data repos
  └── config/            7 configuration classes

  src/test/java/
  └── service/          18 unit test suites

  ───────────────────────────
  All 546 functions mapped
  All 2,032 data types converted
  184 SQL statements → JPA queries
  267 CICS calls → REST endpoints
  Judge verification: PASS (0.97)`,
    logs: [
      { agent: "Cartographer", msg: "Scanning 61 COBOL source files..." },
      { agent: "Cartographer", msg: "Parsing CICS online programs (23)..." },
      { agent: "Cartographer", msg: "Parsing batch programs (18)..." },
      { agent: "Cartographer", msg: "Resolving copybook dependencies..." },
      { agent: "Cartographer", msg: "Building cross-module call graph..." },
      { agent: "Specular", msg: "Extracting 546 business functions..." },
      { agent: "Specular", msg: "Mapping 2,032 data type definitions..." },
      { agent: "Specular", msg: "Tracing 708 inter-module call edges..." },
      { agent: "Architect", msg: "Generating 12 REST controllers..." },
      { agent: "Architect", msg: "Generating 18 service classes..." },
      { agent: "Architect", msg: "Creating 15 JPA entity models..." },
      { agent: "Architect", msg: "Creating 8 Spring Data repositories..." },
      { agent: "Architect", msg: "Generating 18 test suites..." },
      { agent: "Judge", msg: "Verifying semantic equivalence..." },
      { agent: "Judge", msg: "Score: 0.97 — PASS" },
    ],
    summary: "29 modules, 546 functions, 2,032 data types",
    metrics: [
      { value: "61", label: "files" },
      { value: "546", label: "functions" },
      { value: "2,032", label: "data types" },
      { value: "318ms", label: "processing", green: true },
    ],
  },

  /* ── 6. Perl → Java ── */
  {
    label: "Perl (Bugzilla)",
    legacyFile: "Bugzilla.pm",
    modernFile: "BugzillaService.java",
    legacy: `package Bugzilla;
use strict;
use warnings;

use Bugzilla::DB;
use Bugzilla::User;
use Bugzilla::Bug;

our $VERSION = '5.0';

sub new {
    my ($class, %params) = @_;
    my $self = {
        dbh    => undef,
        user   => undef,
        cgi    => $params{cgi},
    };
    return bless $self, $class;
}

sub dbh {
    my ($self) = @_;
    $self->{dbh} ||= Bugzilla::DB->connect;
    return $self->{dbh};
}

sub login {
    my ($self, $user, $pass) = @_;
    my $dbh = $self->dbh;
    my $sth = $dbh->prepare(
        "SELECT * FROM profiles
         WHERE login_name = ?"
    );
    $sth->execute($user);
    my $row = $sth->fetchrow_hashref;
    return unless $row;
    return Bugzilla::User->new($row);
}

sub file_bug {
    my ($self, %bug_data) = @_;
    open(my $fh, '>>', 'buglog.txt');
    print $fh "New bug: $bug_data{summary}\\n";
    close($fh);
    return Bugzilla::Bug->create(%bug_data);
}

1;`,
    modern: `@Service
public class BugzillaService {

    private final DataSource dataSource;
    private final UserRepository userRepo;
    private final BugRepository bugRepo;

    public BugzillaService(DataSource ds,
            UserRepository userRepo,
            BugRepository bugRepo) {
        this.dataSource = ds;
        this.userRepo = userRepo;
        this.bugRepo = bugRepo;
    }

    public Optional<User> login(
            String username, String password) {
        return userRepo.findByLoginName(username)
            .filter(u -> passwordEncoder
                .matches(password, u.getPassword()));
    }

    public Bug fileBug(BugCreateRequest req) {
        log.info("New bug: {}", req.getSummary());

        var bug = Bug.builder()
            .summary(req.getSummary())
            .description(req.getDescription())
            .reporter(req.getReporter())
            .createdAt(Instant.now())
            .build();

        return bugRepo.save(bug);
    }
}`,
    logs: [
      { agent: "Cartographer", msg: "Parsing Perl package..." },
      { agent: "Cartographer", msg: "Resolving use/require dependencies..." },
      { agent: "Cartographer", msg: "Extracting sub declarations..." },
      { agent: "Cartographer", msg: "Detecting DBI database calls..." },
      { agent: "Specular", msg: "Mapping bless → constructor pattern..." },
      { agent: "Specular", msg: "Extracting method call chains..." },
      { agent: "Specular", msg: "Detecting file I/O (open/close)..." },
      { agent: "Architect", msg: "Generating Spring @Service class..." },
      { agent: "Architect", msg: "Converting DBI → JPA Repository..." },
      { agent: "Architect", msg: "Mapping file I/O → SLF4J logging..." },
      { agent: "Judge", msg: "Verifying semantic equivalence..." },
      { agent: "Judge", msg: "Score: 0.94 — PASS" },
    ],
    summary: "1 module, 4 functions, 5 data types",
    metrics: [
      { value: "1", label: "module" },
      { value: "4", label: "functions" },
      { value: "5", label: "data types" },
      { value: "3ms", label: "processing", green: true },
    ],
  },

  /* ── 7. Fortran → Java ── */
  {
    label: "Fortran (Scientific)",
    legacyFile: "linalg.f90",
    modernFile: "LinearAlgebraService.java",
    legacy: `module linear_algebra
  use iso_fortran_env
  implicit none

  integer, parameter :: DP = real64
  real(DP), parameter :: TOL = 1.0e-10_DP

contains

  subroutine mat_mult(A, B, C, m, n, k)
    integer, intent(in) :: m, n, k
    real(DP), intent(in) :: A(m, k), B(k, n)
    real(DP), intent(out) :: C(m, n)
    integer :: i, j, l

    do j = 1, n
      do i = 1, m
        C(i, j) = 0.0_DP
        do l = 1, k
          C(i, j) = C(i, j) + A(i, l) * B(l, j)
        end do
      end do
    end do
  end subroutine mat_mult

  function dot_product_custom(x, y, n) result(dp)
    integer, intent(in) :: n
    real(DP), intent(in) :: x(n), y(n)
    real(DP) :: dp
    integer :: i

    dp = 0.0_DP
    do i = 1, n
      dp = dp + x(i) * y(i)
    end do
  end function dot_product_custom

  subroutine solve_system(A, b, x, n)
    integer, intent(in) :: n
    real(DP), intent(inout) :: A(n, n)
    real(DP), intent(in) :: b(n)
    real(DP), intent(out) :: x(n)

    call gaussian_elimination(A, b, x, n)
  end subroutine solve_system

end module linear_algebra`,
    modern: `@Service
public class LinearAlgebraService {

    private static final double TOL = 1.0e-10;

    public double[][] matMult(
            double[][] a, double[][] b,
            int m, int n, int k) {
        double[][] c = new double[m][n];

        for (int j = 0; j < n; j++) {
            for (int i = 0; i < m; i++) {
                c[i][j] = 0.0;
                for (int l = 0; l < k; l++) {
                    c[i][j] += a[i][l] * b[l][j];
                }
            }
        }
        return c;
    }

    public double dotProduct(
            double[] x, double[] y, int n) {
        double dp = 0.0;
        for (int i = 0; i < n; i++) {
            dp += x[i] * y[i];
        }
        return dp;
    }

    public double[] solveSystem(
            double[][] a, double[] b, int n) {
        return gaussianElimination(a, b, n);
    }
}`,
    logs: [
      { agent: "Cartographer", msg: "Parsing Fortran module..." },
      { agent: "Cartographer", msg: "Resolving USE dependencies..." },
      { agent: "Cartographer", msg: "Extracting subroutine/function defs..." },
      { agent: "Cartographer", msg: "Mapping intent(in/out/inout) params..." },
      { agent: "Specular", msg: "Detecting array operations..." },
      { agent: "Specular", msg: "Mapping DO loops → for loops..." },
      { agent: "Specular", msg: "Extracting CALL graph edges..." },
      { agent: "Architect", msg: "Generating Spring @Service class..." },
      { agent: "Architect", msg: "Converting real(DP) → double..." },
      { agent: "Architect", msg: "Mapping parameters → static final..." },
      { agent: "Judge", msg: "Verifying numeric equivalence..." },
      { agent: "Judge", msg: "Score: 0.96 — PASS" },
    ],
    summary: "1 module, 3 functions, 4 data types",
    metrics: [
      { value: "1", label: "module" },
      { value: "3", label: "functions" },
      { value: "4", label: "data types" },
      { value: "2ms", label: "processing", green: true },
    ],
  },
];

/* ═══════════════════════════════════════════════════════ */

type Phase = "idle" | "processing" | "success";

export default function DemoPage() {
  const [activeIdx, setActiveIdx] = useState(0);
  const [phase, setPhase] = useState<Phase>("idle");
  const [visibleLogs, setVisibleLogs] = useState(0);
  const [scanProgress, setScanProgress] = useState(0);
  const logRef = useRef<HTMLDivElement>(null);
  const intervalsRef = useRef<ReturnType<typeof setInterval>[]>([]);
  const timeoutsRef = useRef<ReturnType<typeof setTimeout>[]>([]);

  const { theme, toggle: toggleTheme } = useTheme();
  const ex = examples[activeIdx];

  /* Auto-scroll logs */
  useEffect(() => {
    if (logRef.current) logRef.current.scrollTop = logRef.current.scrollHeight;
  }, [visibleLogs]);

  /* Cleanup on unmount or tab switch */
  const clearTimers = useCallback(() => {
    intervalsRef.current.forEach(clearInterval);
    timeoutsRef.current.forEach(clearTimeout);
    intervalsRef.current = [];
    timeoutsRef.current = [];
  }, []);

  const reset = useCallback(() => {
    clearTimers();
    setPhase("idle");
    setVisibleLogs(0);
    setScanProgress(0);
  }, [clearTimers]);

  const switchTab = (i: number) => {
    if (i === activeIdx) return;
    reset();
    setActiveIdx(i);
  };

  const runMigration = useCallback(() => {
    if (phase === "processing") return;
    setPhase("processing");
    setVisibleLogs(0);
    setScanProgress(0);

    let scan = 0;
    const scanId = setInterval(() => {
      scan += 2;
      setScanProgress(Math.min(scan, 100));
      if (scan >= 100) clearInterval(scanId);
    }, 50);
    intervalsRef.current.push(scanId);

    let logIdx = 0;
    const logId = setInterval(() => {
      logIdx++;
      setVisibleLogs(logIdx);
      if (logIdx >= ex.logs.length) {
        clearInterval(logId);
        const tid = setTimeout(() => setPhase("success"), 400);
        timeoutsRef.current.push(tid);
      }
    }, 300);
    intervalsRef.current.push(logId);
  }, [phase, ex.logs.length]);

  useEffect(() => () => clearTimers(), [clearTimers]);

  return (
    <div className="min-h-screen flex flex-col" style={{ backgroundColor: "var(--color-bg)" }}>
      {/* ── Header ── */}
      <header
        className="flex items-center gap-4 px-4 md:px-8 py-4"
        style={{ borderBottom: "1px solid var(--color-border)", backgroundColor: "var(--color-surface-alt)" }}
      >
        <Link href="/" className="flex items-center gap-2 font-mono text-xs transition-colors" style={{ color: "var(--color-text-secondary)" }}>
          <ArrowLeft size={14} /> Back
        </Link>
        <div className="h-4 w-px" style={{ backgroundColor: "var(--color-border)" }} />
        <span className="font-mono text-[10px] font-semibold tracking-[2px]" style={{ color: "var(--color-accent)" }}>
          LIVE MIGRATION DEMO
        </span>
        <button
          onClick={toggleTheme}
          className="ml-auto p-2 transition-colors"
          style={{ color: "var(--color-text-secondary)" }}
          aria-label="Toggle theme"
        >
          {theme === "dark" ? <Sun size={16} /> : <Moon size={16} />}
        </button>
      </header>

      {/* ── Main ── */}
      <div className="flex-1 flex items-center justify-center px-4 md:px-8 py-8 md:py-12">
        <div className="w-full max-w-[1100px]">
          {/* Title + action */}
          <div className="flex flex-col md:flex-row md:items-end md:justify-between gap-4 mb-6">
            <div>
              <h1 className="text-xl md:text-2xl font-bold" style={{ color: "var(--color-text)" }}>
                Legacy → Java Migration
              </h1>
              <p className="font-mono text-xs mt-1" style={{ color: "var(--color-text-secondary)" }}>
                Watch Anvil transform COBOL, Perl, or Fortran in real-time
              </p>
            </div>

            {phase === "idle" ? (
              <button onClick={runMigration} className="flex items-center gap-2 font-mono text-xs font-semibold px-5 py-2 transition hover:brightness-110 self-start md:self-auto" style={{ backgroundColor: "var(--color-accent)", color: "#000" }}>
                <Play size={14} /> Start Migration
              </button>
            ) : phase === "processing" ? (
              <span className="flex items-center gap-2 font-mono text-xs" style={{ color: "var(--color-accent)" }}>
                <Loader2 size={14} className="animate-spin" /> Processing...
              </span>
            ) : (
              <button onClick={reset} className="flex items-center gap-2 font-mono text-xs font-semibold px-5 py-2 transition hover:brightness-110 self-start md:self-auto" style={{ backgroundColor: "var(--color-accent)", color: "#000" }}>
                <RotateCcw size={14} /> Run Again
              </button>
            )}
          </div>

          {/* ── Tabs ── */}
          <div className="flex w-full overflow-x-auto" style={{ borderBottom: "1px solid var(--color-border)" }}>
            {examples.map((e, i) => (
              <button
                key={i}
                onClick={() => switchTab(i)}
                className="font-mono text-[11px] md:text-xs px-4 md:px-6 py-3 transition-colors whitespace-nowrap shrink-0"
                style={{
                  color: i === activeIdx ? "var(--color-text)" : "var(--color-text-secondary)",
                  backgroundColor: i === activeIdx ? "var(--color-surface)" : "transparent",
                  borderBottom: i === activeIdx ? "2px solid var(--color-accent)" : "1px solid var(--color-border)",
                }}
              >
                {e.label}
              </button>
            ))}
            <div className="flex-1" style={{ borderBottom: "1px solid var(--color-border)" }} />
          </div>

          {/* ── Split panels ── */}
          <div className="flex flex-col md:flex-row w-full" style={{ border: "1px solid var(--color-border)", borderTop: "none", minHeight: "500px" }}>
            {/* Left — Terminal */}
            <div className="md:w-[340px] flex flex-col shrink-0" style={{ backgroundColor: "var(--color-code-bg)", borderRight: "1px solid var(--color-border)" }}>
              <div className="flex items-center gap-2 px-4 py-3" style={{ borderBottom: "1px solid var(--color-border)" }}>
                <Terminal size={13} style={{ color: "var(--color-text-secondary)" }} />
                <span className="font-mono text-[11px] font-semibold" style={{ color: "var(--color-text-secondary)" }}>Pipeline Output</span>
              </div>

              <div ref={logRef} className="flex-1 p-4 overflow-y-auto font-mono text-[11px] leading-[1.8] flex flex-col gap-1" style={{ minHeight: "200px", maxHeight: "452px" }}>
                {phase === "idle" && (
                  <span style={{ color: "var(--color-text-muted)" }}>Waiting for migration to start...</span>
                )}

                <AnimatePresence>
                  {ex.logs.slice(0, visibleLogs).map((entry, i) => (
                    <motion.div key={`${activeIdx}-${i}`} initial={{ opacity: 0, x: -8 }} animate={{ opacity: 1, x: 0 }} transition={{ duration: 0.2 }} className="flex gap-2">
                      <span style={{ color: "var(--color-accent)" }}>[{entry.agent}]</span>
                      <span style={{ color: "var(--color-text-secondary)" }}>{entry.msg}</span>
                    </motion.div>
                  ))}
                </AnimatePresence>

                {phase === "success" && (
                  <motion.div initial={{ opacity: 0 }} animate={{ opacity: 1 }} transition={{ delay: 0.2 }} className="flex items-center gap-2 mt-2 pt-2" style={{ borderTop: "1px solid var(--color-border)" }}>
                    <CheckCircle2 size={13} style={{ color: "var(--color-green)" }} />
                    <span className="font-semibold" style={{ color: "var(--color-green)" }}>Migration complete</span>
                  </motion.div>
                )}
              </div>
            </div>

            {/* Right — Code Editor */}
            <div className="flex-1 flex flex-col" style={{ backgroundColor: "var(--color-bg)" }}>
              <div className="flex items-center gap-2 px-4 py-3" style={{ borderBottom: "1px solid var(--color-border)" }}>
                <Code2 size={13} style={{ color: "var(--color-text-secondary)" }} />
                <span className="font-mono text-[11px] font-semibold" style={{ color: "var(--color-text-secondary)" }}>
                  {phase === "success" ? ex.modernFile : ex.legacyFile}
                </span>
                {phase === "success" && (
                  <span className="ml-auto font-mono text-[10px] px-2 py-0.5" style={{ color: "var(--color-green)", border: "1px solid var(--color-green)" }}>
                    TRANSFORMED
                  </span>
                )}
              </div>

              <div className="flex-1 relative p-4 overflow-auto">
                {/* Scan overlay */}
                <AnimatePresence>
                  {phase === "processing" && (
                    <motion.div initial={{ opacity: 0 }} animate={{ opacity: 1 }} exit={{ opacity: 0 }} className="absolute inset-0 z-10 pointer-events-none">
                      <motion.div className="absolute left-0 right-0 h-[2px]" style={{ backgroundColor: "var(--color-accent)", boxShadow: "0 0 12px var(--color-accent), 0 0 30px var(--color-accent-dim)", top: `${scanProgress}%` }} />
                      <div className="absolute left-0 right-0 top-0 transition-all" style={{ height: `${scanProgress}%`, background: "linear-gradient(180deg, var(--color-accent-dim) 0%, transparent 100%)" }} />
                    </motion.div>
                  )}
                </AnimatePresence>

                {/* Code */}
                <AnimatePresence mode="wait">
                  {phase !== "success" ? (
                    <motion.pre key={`legacy-${activeIdx}`} initial={{ opacity: 1 }} exit={{ opacity: 0, filter: "blur(4px)", scale: 0.98 }} transition={{ duration: 0.4 }} className="font-mono text-[11px] md:text-xs leading-[1.8] whitespace-pre" style={{ color: "var(--color-text-secondary)" }}>
                      {addLineNumbers(ex.legacy)}
                    </motion.pre>
                  ) : (
                    <motion.pre key={`modern-${activeIdx}`} initial={{ opacity: 0, filter: "blur(4px)", scale: 0.98 }} animate={{ opacity: 1, filter: "blur(0px)", scale: 1 }} transition={{ duration: 0.5 }} className="font-mono text-[11px] md:text-xs leading-[1.8] whitespace-pre" style={{ color: "var(--color-code-green)" }}>
                      {addLineNumbers(ex.modern)}
                    </motion.pre>
                  )}
                </AnimatePresence>
              </div>
            </div>

            {/* Metrics sidebar */}
            <div className="flex md:flex-col gap-4 p-4 md:p-5 md:w-[180px] shrink-0" style={{ backgroundColor: "var(--color-surface)", borderLeft: "1px solid var(--color-border)" }}>
              <span className="font-mono text-[10px] font-semibold tracking-[2px] hidden md:block" style={{ color: "var(--color-accent)" }}>METRICS</span>
              <div className="flex md:flex-col gap-4 flex-1">
                {ex.metrics.map((m, i) => (
                  <div key={i} className="flex flex-col gap-0.5">
                    <span className={`font-mono text-lg md:text-xl font-bold ${m.green ? "" : ""}`} style={{ color: m.green ? "var(--color-green)" : "var(--color-text)" }}>{m.value}</span>
                    <span className="font-mono text-[10px]" style={{ color: "var(--color-text-secondary)" }}>{m.label}</span>
                  </div>
                ))}
              </div>
            </div>
          </div>

          {/* Status bar */}
          <div className="flex items-center justify-between px-4 py-2 font-mono text-[10px]" style={{ backgroundColor: "var(--color-surface)", borderLeft: "1px solid var(--color-border)", borderRight: "1px solid var(--color-border)", borderBottom: "1px solid var(--color-border)", color: "var(--color-text-muted)" }}>
            <span>anvil v0.1.0</span>
            <span>
              {phase === "idle" && "Ready"}
              {phase === "processing" && `Migrating... ${Math.min(scanProgress, 100)}%`}
              {phase === "success" && `✓ Migration complete — ${ex.summary}`}
            </span>
          </div>
        </div>
      </div>
    </div>
  );
}

/* ── Helpers ── */
function addLineNumbers(code: string): string {
  return code.split("\n").map((line, i) => `${String(i + 1).padStart(3, " ")}  ${line}`).join("\n");
}
