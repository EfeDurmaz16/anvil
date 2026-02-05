# Anvil Technical Readiness Audit

**Date**: 2026-02-05
**Auditor**: Senior Staff Engineer / QA Lead (automated deep audit)
**Scope**: Full codebase analysis for Nokia internal demo and Turk Telekom pilot readiness

---

## 1. HIGH-LEVEL READINESS SUMMARY

- **The core pipeline (Cartographer -> Specular -> Architect -> Judge) is real, functional, and works end-to-end in both LLM and LLM-free modes.** This is not vaporware. All 4 agents, 3 source parsers, and 4 target generators contain genuine implementations.
- **The codebase was built in 4 days (Feb 1-4, 2026) with 20 commits.** This pace explains both the impressive breadth and the significant gaps in testing and integration.
- **~2,000+ lines of infrastructure code (observability, tracing, health, secrets, audit) are implemented but never wired into either binary.** This creates a false sense of operational maturity.
- **All LLM prompts in target plugins are hardcoded to "COBOL"**, even when processing Perl or Fortran source code. This will confuse the LLM and degrade output quality for non-COBOL demos.
- **Java target is missing `anvil.manifest.json` and fixture runner**, breaking the harness pipeline for Java. The other 3 targets (TypeScript, Python, Go) are fully functional.

### Readiness Scores

| Scenario | Score | Justification |
|----------|-------|---------------|
| (a) Nokia internal demo on synthetic/safe legacy code | **6/10** | Happy path works for COBOL->TS/Python/Go with `provider: none`. Must avoid Java target and stick to scripted scenarios. |
| (b) Serious PoC with real telecom/bank module | **3/10** | No CI, no end-to-end tests, untested LLM paths, no fixture recording, COBOL-only LLM prompts, missing config validation. |

### Go / No-Go Decisions

| Pilot | Decision | Conditions |
|-------|----------|------------|
| **Nokia internal pilot** | **GO WITH CONDITIONS** | Script the demo path. Use COBOL->TypeScript or COBOL->Go. Avoid Java. Avoid Perl/Fortran with LLM enabled. Pre-build fixtures. Have a rollback plan if parsing fails mid-demo. |
| **First telco operator pilot** | **NO-GO** | Too many untested paths. No CI. No fixture recording. COBOL-only prompts. Need 2-4 weeks of hardening first. |
| **First bank pilot** | **NO-GO** | Banks require audit trails, compliance logging, and validated proof packs. The audit logger exists but is never initialized. DB diff has a SQL injection vector. PII masking is untested in production paths. |

---

## 2. PIPELINE AUDIT (Cartographer -> Specular -> Architect -> Judge)

### 2.1 Cartographer

**Implementation location**: `internal/agents/cartographer/cartographer.go`, `internal/plugins/source/cobol/`, `internal/plugins/source/perl/`, `internal/plugins/source/fortran/`

**Implementation reality vs spec**: MATCHES. Cartographer reads source files, dispatches to the correct source plugin based on `--source` flag, builds a `SemanticGraph` IR, and optionally stores it in Neo4j. All three source languages have real parsers with real tests.

**Robustness & failure modes**:
- File-not-found and read errors are properly propagated
- `filepath.Walk` is **all-or-nothing**: one unreadable file in a directory aborts the entire parse
- Neo4j storage failure is non-fatal (warning, continues)
- Fallback file extensions default to `.cbl/.cob/.cpy` (COBOL) when a source plugin doesn't implement `FileExtensionsProvider` -- a trap for new plugins

**Language coverage**:

| Source | Status | Parser LOC | Tests | Key Features |
|--------|--------|-----------|-------|-------------|
| **COBOL** | Production-ready | ~560 | 6 (incl. CardDemo 100+ fn) | Divisions, PIC clauses, COMP/BINARY, copybooks, PERFORM/CALL, EXEC CICS/SQL, EVALUATE, 88-levels |
| **Perl** | Functional | ~460 | 7 | Package/sub, DBI, file I/O, OO method calls, variables. Missing: regex, Moose/Moo, eval, here-docs |
| **Fortran** | Functional | ~730 | 11 | F77-F2008, modules, USE resolution, all data types incl. COMPLEX, CONTAINS, free/fixed form. Missing: COMMON, INTERFACE |

**LLM-optional behavior**: Cartographer has **zero LLM dependency**. Pure regex-based parsing. Works perfectly with `provider: none`.

### 2.2 Specular

**Implementation location**: `internal/agents/specular/specular.go` (110 lines, single file)

**Implementation reality vs spec**: PARTIAL. Specular does extract business rules via LLM, but it's a thin single-pass extraction with no RAG, no iterative refinement, and no cross-function analysis.

**Robustness & failure modes**:
- Per-function LLM errors are non-fatal (adds error, continues to next)
- Non-JSON LLM responses are gracefully wrapped as a single low-confidence rule (0.5)
- With `provider: none`: clean passthrough, `StatusPassthrough`, zero rules extracted
- **System prompt is hardcoded to "COBOL business rule extraction expert"** -- incorrect for Perl/Fortran

**Tests**: **ZERO tests**. The Specular agent has no test file at all.

### 2.3 Architect

**Implementation location**: `internal/agents/architect/architect.go` (75 lines), `internal/plugins/target/typescript/` (457 lines), `internal/plugins/target/python/` (310 lines), `internal/plugins/target/golang/` (323 lines), `internal/plugins/target/java/` (5 files, ~240 lines total)

**Implementation reality vs spec**: MOSTLY MATCHES. All four targets implement both template-only and LLM-enhanced generation. Emits `anvil.manifest.json` for TS/Python/Go. **Java is missing manifest and fixture runner.**

**Target coverage**:

| Target | Scaffold | Template Gen | LLM Gen | Manifest | Runner | Tests |
|--------|----------|-------------|---------|----------|--------|-------|
| TypeScript | 5 files | Yes | Per-function | Yes | Yes | 3 |
| Python | 5 files | Yes | Per-function | Yes | Yes | 3 |
| Go | 3 files | Yes | Per-function | Yes | Yes | 3 |
| **Java** | 2 files | Yes | Per-module (coarser) | **MISSING** | **MISSING** | 3 |

**Critical issue**: All four target plugins hardcode "COBOL" in LLM prompts:
- `"You are a COBOL to TypeScript migration expert"` (even for Perl->TS)
- `"Original COBOL Body:\n..."` (even for Fortran source)
- The `Module.Language` field already carries the correct source language but is never read by target plugins

**LLM-optional behavior**: All targets check `if provider != nil` and fall back to template stubs. Template mode produces compilable code with `// TODO: implement` stubs.

**Tests**: **ZERO tests for the Architect agent itself** (`architect.go`). Target plugins have unit tests but only for template mode (nil LLM).

### 2.4 Judge

**Implementation location**: `internal/agents/judge/judge.go` (225 lines), `internal/agents/judge/scoring.go` (220 lines)

**Implementation reality vs spec**: PARTIAL. The Judge does LLM-based semantic verification per function. However:
- The sophisticated gate-based scoring (`scoring.go`) is **NOT wired into** `judge.go`'s `Run()` method
- Judge uses a simpler deduction model (start at 1.0, deduct 0.2 per failure)
- Gate scoring is only used in the Temporal `HarnessActivity`

**Robustness & failure modes**:
- Per-function errors are non-fatal (score deducted, continues)
- Robust JSON parsing: tries strict unmarshal, then extracts `{...}` from markdown-wrapped responses
- With `provider: none`: auto-passes with score 1.0 (potentially misleading in demos)

**Retry loop (Judge -> Architect)**: Implemented in Temporal workflow. `maxRetries = 2` (3 total attempts). Combined score = (Judge + Harness) / 2, threshold 0.8. Also implemented in CLI `runPipeline()`.

**Tests**: 2 tests for Judge agent, 13 tests for gate scoring. Gate scoring tests are comprehensive.

---

## 3. CLI & CONFIG READINESS

### `anvil run`

| Step | Works? | Notes |
|------|--------|-------|
| Detect source language | YES | Via `--source` flag, with alias normalization (ts->typescript, py->python) |
| Traverse input directories | YES | `filepath.Walk` with plugin-provided file extensions |
| Invoke Cartographer/Specular/Architect in sequence | YES | With retry loop (max 3 attempts, threshold 0.8) |
| Write output code + manifest | YES | For TS/Python/Go. Java writes code but no manifest. |

### `anvil harness run`

| Step | Works? | Notes |
|------|--------|-------|
| Load fixtures (JSONL) | YES | With validation (missing kind/name detected, line numbers in errors) |
| Execute via `anvil.manifest.json` | YES | ManifestRunner pipes fixture JSON to stdin, parses ActualOutput from stdout |
| Capture outputs and diffs | YES | HTTP diff with JSON normalization, header/field ignoring |
| Assemble proof pack | YES | `summary.json` with per-fixture pass/fail, timestamps, counts |

### `anvil.yaml` Configuration

| Field | Status | Notes |
|-------|--------|-------|
| `llm.provider` | LIVE | Controls provider selection |
| `llm.model` | LIVE | Passed to provider |
| `llm.api_key` | LIVE | Passed to provider |
| `llm.base_url` | LIVE | Custom endpoint override |
| `llm.temperature` | **DEAD** | Parsed but never read by any code |
| `llm.max_tokens` | **DEAD** | Parsed but never read by any code |
| `graph.*` | **DEAD (CLI)** | Neo4j never instantiated in CLI path |
| `vector.*` | **DEAD (CLI)** | Qdrant never instantiated in CLI path |
| `temporal.*` | LIVE (worker only) | Required for worker binary |
| `log.level` | **DEAD** | Parsed but never consumed |
| `log.format` | **DEAD** | Parsed but never consumed |

**9 of 17 config fields are dead or unused in the CLI path.** Defaults are sane: `provider: none` allows LLM-free operation. CLI is resilient to missing config (uses zero-value defaults). Worker crashes on missing config (`log.Fatalf`).

**No config validation exists.** Invalid `temperature`, port ranges, or URL formats pass silently to downstream code.

---

## 4. LANGUAGE-AGNOSTICITY & PLUGIN SYSTEM

### Plugin Architecture: WELL DESIGNED

The plugin system is genuinely extensible:
- `SourcePlugin` interface: 3 methods (`Language()`, `Parse()`, `ResolveDependencies()`)
- `TargetPlugin` interface: 3 methods (`Language()`, `Generate()`, `Scaffold()`)
- Thread-safe registry with explicit registration in `cmd/anvil/main.go`
- Adding a new target (e.g., Rust) requires: 1 new package + 1 import + 1 registration line

**Estimated effort for new target**: 1-2 days for stub, 1-2 weeks for LLM-assisted generation.

### IR: GENUINELY LANGUAGE-AGNOSTIC

The `ir.SemanticGraph` in `internal/ir/graph.go` has no COBOL-specific fields. Language-specific metadata is stored in `Metadata map[string]string` escape hatches (e.g., `"level"`, `"usage"` for COBOL; `"sigil"` for Perl; `"intent"` for Fortran). Seven `TypeKind` values cover all current languages.

### Where "Language-Agnostic" Breaks Down

1. **All target plugin LLM prompts say "COBOL"** -- this is the single biggest language-agnosticity violation
2. **Specular prompt says "COBOL business rule extraction expert"** -- wrong for Perl/Fortran
3. **Cartographer fallback extensions default to `.cbl/.cob/.cpy`** -- trap for new plugins
4. **CLI defaults**: `--source cobol --target java` -- reasonable but biased
5. **Worker binary only registers COBOL source plugin** (`cmd/worker/main.go` line 38) -- Perl/Fortran jobs will fail on Temporal workers
6. **All test data is COBOL-dominant** -- 8 COBOL test projects, 1 Perl file, 2 Fortran files
7. **Duplicated utilities**: `dedup()`, `toPascalCase()`, `toCamelCase()`, `toSnakeFile()` independently reimplemented in 7+ plugin packages

---

## 5. TEST COVERAGE & QA GAPS

### Test Inventory

| Category | Count | Packages |
|----------|-------|----------|
| Source parser tests | 24 | cobol (6), perl (7), fortran (11) |
| Target plugin tests | 12 | java (3), typescript (3), python (3), go (3) |
| Agent tests | 16 | cartographer (1), judge (2), scoring (13) |
| Harness tests | ~35 | fixtures, diff, PII, runner, proof pack |
| Infrastructure tests | ~115 | secrets (~25), audit (~30), metrics (~25), tracing (~20), health (~20), shutdown (~20) |
| LLM tests | 12 | rate limiter (12) |
| Registry tests | 1 | registry (1) |
| **Total** | **~215** | **19 test files** |

### Critical Missing Tests

| Gap | Risk | Impact |
|-----|------|--------|
| **No end-to-end pipeline test** | HIGH | The core product flow is completely untested as a unit |
| **No CI pipeline** | HIGH | Tests are manual-only, no automated gating |
| **Architect agent: 0 tests** | HIGH | Code generation orchestration is untested |
| **Specular agent: 0 tests** | HIGH | Business rule extraction is untested |
| **Temporal workflows: 0 tests** | HIGH | Production orchestration is untested |
| **LLM providers (Anthropic/OpenAI): 0 tests** | MEDIUM | HTTP client code is untested |
| **LLM retry logic: 0 tests** | MEDIUM | Exponential backoff is untested |
| **LLM factory: 0 tests** | MEDIUM | Provider creation is untested |
| **IR package: 0 tests** | MEDIUM | Central data structure is untested |
| **CLI entry points: 0 tests** | MEDIUM | Command parsing/dispatch is untested |
| **LLM-assisted generation: 0 tests** | HIGH | All target plugin tests use nil LLM |
| **Landing site: 0 tests** | LOW | Marketing site, not product code |

### Existing Example/Fixture Data

- **COBOL**: 8 test projects including AWS CardDemo (32 programs) and CobolCraft (100+ files). Production-grade.
- **Perl**: 1 file (Bugzilla.pm, ~730 lines). Realistic but insufficient.
- **Fortran**: 2 files (matrix_multiply.f90 + stdlib_math.f90). Minimal.
- **Fixtures**: 1 JSONL file with 3 HTTP fixtures. Insufficient for serious validation.
- **Proof packs**: Zero example outputs in the repo.
- **Pre-generated outputs**: `testdata/output/` contains hand-crafted demo JSON that uses a different schema than actual pipeline output.

---

## 6. FIRST DEMO / PILOT READINESS

### Nokia Internal Demo

**What we can confidently demo today:**
- COBOL parsing with real CardDemo code: `anvil run --source cobol --target typescript --input testdata/cobol/calculator.cbl --output /tmp/demo`
- IR visualization (the semantic graph JSON output)
- Template-based code generation (no LLM needed) for TypeScript, Python, Go
- Fixture-based validation with the sample JSONL
- Pipeline metrics summary output
- The landing page with interactive demo component

**What we can demo with scripted paths:**
- LLM-enhanced generation (requires valid API key, network access, specific model)
- Proof pack generation (works but no pre-built demo artifacts)
- Multi-file COBOL project parsing (CardDemo -- requires git submodule init)

**What we should avoid showing:**
- Java target (missing manifest, harness will fail)
- Perl or Fortran with LLM enabled (prompts say "COBOL")
- DB diff feature (no Postgres driver imported, will crash)
- `anvil harness record` (prints instructions only, not implemented)
- Temporal workflow mode (requires infrastructure, adds complexity with no demo benefit)
- Neo4j/Qdrant integration (wired but never connected in CLI)

**Suggested demo scenario:**
> Take the COBOL calculator (`testdata/cobol/calculator.cbl`, ~50 LOC), run `anvil run --source cobol --target typescript --input testdata/cobol/calculator.cbl --output /tmp/demo`. Show the generated TypeScript with proper type mappings and `anvil.manifest.json`. Then run `anvil harness run --fixtures testdata/fixtures/sample.jsonl --code /tmp/demo` and show the proof pack.

**Top 5 failure risks for this scenario:**
1. **LLM rate limiting** (if using LLM): Rate limiter code exists but is never wired in. Could get 429 storms. **Mitigation**: Use `provider: none` for the demo.
2. **TypeScript compilation**: Generated code may not compile if `tsc` is not installed on the demo machine. **Mitigation**: Pre-install Node.js + TypeScript, or use Python target.
3. **go.mod version mismatch**: `go 1.25.1` in go.mod is a non-existent Go version. **Mitigation**: Fix to `go 1.23` before the demo.
4. **Fixture/code mismatch**: Sample fixtures assume a banking API, but the calculator COBOL produces arithmetic. **Mitigation**: Create matching calculator fixtures.
5. **Missing git submodules**: CardDemo test data requires `git submodule init`. **Mitigation**: Use the inline `testdata/cobol/` files instead.

### Turk Telekom Pilot

**Additional requirements before touching real billing/provisioning code:**
- Fix COBOL-hardcoded LLM prompts in all target plugins
- Add config validation (especially for LLM settings)
- Wire the audit logger for compliance trail
- Wire rate limiting to prevent API cost blowouts
- Create telecom-specific fixtures (CDR processing, billing calculations)
- Test with >1K LOC COBOL modules to verify parser handles real complexity
- Fix the SQL injection vector in `harness/dbdiff.go`
- Add CI pipeline with automated testing

**Minimum checklist before piloting on a real module:**
- [ ] End-to-end test passing for COBOL->{target} on a representative module
- [ ] LLM-enhanced generation tested with their preferred provider
- [ ] Fixture recording mechanism (currently stub-only)
- [ ] Audit logging active and writing to persistent storage
- [ ] Rate limiting wired for LLM calls
- [ ] Config validation preventing silent failures
- [ ] Proof pack format reviewed and approved by their architecture team
- [ ] Rollback/undo procedure documented (Anvil only generates, never modifies source)

---

## 7. RECOMMENDED TEST PLAN (NEXT 2-4 WEEKS)

### Step 1: Local Synthetic Tests (Week 1)

**COBOL - Calculator with Arithmetic:**
- Source: `testdata/cobol/calculator.cbl` (already exists)
- Behavior: ADD, SUBTRACT, MULTIPLY operations
- Fixture: `{"kind":"batch","name":"add-100-50","input":{"num1":100,"num2":50,"op":"add"},"expected":{"result":150}}`
- Assert: Parse extracts 4 paragraphs, codegen produces compilable TS/Python/Go, fixture passes

**Perl - Simple HTTP Handler:**
- Source: Create `testdata/perl/http_handler.pl` (~50 LOC) with a CGI-like request handler
- Behavior: Parse query params, validate, return JSON response
- Fixture: HTTP GET with params, expected JSON response
- Assert: Parse extracts subs and DBI patterns, codegen produces runnable service

**Fortran - Matrix Operations:**
- Source: `testdata/fortran/matrix_multiply.f90` (already exists)
- Behavior: Matrix multiplication with configurable dimensions
- Fixture: `{"kind":"batch","name":"3x3-multiply","input":{"a":[[1,2],[3,4]],"b":[[5,6],[7,8]]},"expected":{"result":[[19,22],[43,50]]}}`
- Assert: Parse extracts subroutines with INTENT params, codegen produces working code

### Step 2: Public Sample Projects (Week 1-2)

**COBOL**: Use the existing `testdata/carddemo/` (AWS CardDemo). Target: parse all 32 programs, generate TypeScript for 3-5 representative modules (COSGN00C sign-on, CBACT01C account view, CBTRN01C transaction).

**Fortran**: GnuCOBOL's test suite or the SciPy `special` functions (Fortran originals). Alternatively, use LAPACK's simple routines (DGEMM, DGESV).

Integration approach: Add `make test-integration` target that:
1. Parses each project with `anvil run --source X --target typescript`
2. Checks that generated code compiles (`tsc --noEmit`)
3. Reports parse coverage (functions found vs. expected)

### Step 3: Nokia-Style Domain Sample (Week 2-3)

Build a simplified telecom rating engine in COBOL (~200-500 LOC):
- CDR (Call Detail Record) parsing: read fixed-length records
- Rate lookup: time-of-day and destination-based pricing
- Discount application: volume discounts, plan-specific rules
- Output: rated CDR with computed charges

Use this to validate:
1. Record-replay: Create 10-20 CDR fixtures with known rated outputs
2. Proof pack: Generate and review the full proof pack
3. Multi-file: Split across 3-4 COBOL programs with copybooks

### Step 4: Automation & CI (Week 2-4)

**GitHub Actions pipeline:**
```yaml
# .github/workflows/ci.yml
- go vet ./...
- go build ./...
- go test ./... -short -count=1
- anvil run --source cobol --target typescript --input testdata/cobol/ --output /tmp/ci-ts
- anvil run --source cobol --target python --input testdata/cobol/ --output /tmp/ci-py
- anvil harness run --fixtures testdata/fixtures/sample.jsonl --code /tmp/ci-ts
```

**Success metrics to track:**
| Metric | Target | Current |
|--------|--------|---------|
| Unit test pass rate | 100% | 100% (all 215 pass) |
| Source languages with integration tests | 3/3 | 0/3 |
| Target languages with compile-check | 4/4 | 0/4 |
| End-to-end pipeline tests | >= 3 | 0 |
| Fixture pass rate | >= 90% | N/A (no fixtures in CI) |
| CI pipeline exists | Yes | No |

---

## 8. TOP 10 RISKS & PRIORITIZED FIXES

### Risk Matrix

| # | Risk | Impact | Likelihood | Audience | Mitigation |
|---|------|--------|------------|----------|------------|
| 1 | **COBOL-hardcoded LLM prompts in target plugins** | LLM generates wrong code for Perl/Fortran demos; audience questions language-agnosticity claim | HIGH | Nokia (technical), Telco CTO | Replace hardcoded "COBOL" with `Module.Language` in 4 target plugin files. ~20 string replacements. **1 hour fix.** |
| 2 | **Java target missing manifest + runner** | Harness fails for Java; audience sees error mid-demo | HIGH | All | Add `anvil.manifest.json` and `AnvilRunner.java` to Java scaffold. **4-8 hour fix.** |
| 3 | **No CI pipeline** | Bad merge breaks everything silently; demo machine has stale code | HIGH | Internal team | Add `.github/workflows/ci.yml` with build + test + lint. **2 hour fix.** |
| 4 | **No end-to-end tests** | Pipeline breaks in integration even though unit tests pass | HIGH | All (trust) | Add 1 E2E test: COBOL->TypeScript->compile->fixture. **4 hour fix.** |
| 5 | **`go.mod` declares `go 1.25.1`** | Build fails on any machine without matching toolchain | MEDIUM | Nokia (trying to build it) | Change to `go 1.23`. **5 minute fix.** |
| 6 | **Rate limiter never wired** | LLM calls have no rate limiting; risk of 429 storms and unexpected API costs | MEDIUM | Telco operator (cost-conscious) | Add `llm.WithRateLimit()` after `factory.Create()`. **30 minute fix.** |
| 7 | **Worker missing Perl/Fortran plugins** | Temporal workflow jobs for Perl/Fortran fail silently | MEDIUM | Nokia (if using Temporal) | Add 2 lines of plugin registration in `cmd/worker/main.go`. **5 minute fix.** |
| 8 | **SQL injection in DB diff** | `fmt.Sprintf` with table names in raw SQL; security-conscious audience flags it | HIGH | Bank CTO | Use parameterized queries or allowlist validation. **2 hour fix.** |
| 9 | **Observability infrastructure is dead code** | Audience asks about monitoring; answer is "it's implemented but not connected" | MEDIUM | Telco CTO, Bank architecture | Wire `InitTracing()`, `InitGlobalAuditLogger()`, health server in main.go. **4 hour fix.** |
| 10 | **No fixture recording** | `anvil harness record` is a stub; audience asks how to capture fixtures from their system | HIGH | All (workflow question) | Either implement HTTP proxy recording or document external tools (mitmproxy, tcpdump). **1-3 day fix.** |

### Silent JSON marshal errors in Temporal activities

`internal/temporal/activities.go` lines 138, 164 use `gOut, _ := json.Marshal(result.Graph)`, silently discarding marshal errors. This can corrupt data passed between activities. **Fix: handle the error. 10 minute fix.**

### "If I were the founder, I would fix these 3 things first"

1. **Fix COBOL-hardcoded prompts** (1 hour) -- This is the most embarrassing bug for a "language-agnostic" platform. Trivial fix, massive credibility impact.

2. **Add CI + 1 E2E test** (half day) -- Without this, every code change is a gamble. One GitHub Actions file + one integration test gives confidence that the demo path works.

3. **Fix Java manifest + go.mod version** (half day) -- These are the two things most likely to cause a live demo failure. The Java gap breaks the harness. The go.mod version breaks the build on other machines.

---

## Appendix: File Reference

### Core Pipeline
- `cmd/anvil/main.go` -- CLI entry point (634 lines, should be split)
- `cmd/worker/main.go` -- Temporal worker (missing Perl/Fortran plugins)
- `internal/agents/agent.go` -- Agent interface + AgentContext + AgentResult
- `internal/agents/cartographer/cartographer.go` -- Source parsing orchestrator
- `internal/agents/specular/specular.go` -- LLM business rule extraction (0 tests)
- `internal/agents/architect/architect.go` -- Code generation orchestrator (0 tests)
- `internal/agents/judge/judge.go` -- LLM semantic verification
- `internal/agents/judge/scoring.go` -- Gate-based scoring (NOT wired into judge.go)

### Source Plugins
- `internal/plugins/source/cobol/` -- 4 files: cobol.go, data_division.go, procedure_division.go, copybook.go
- `internal/plugins/source/perl/perl.go` -- 463 lines
- `internal/plugins/source/fortran/fortran.go` -- 732 lines

### Target Plugins
- `internal/plugins/target/typescript/typescript.go` -- 457 lines (most complete)
- `internal/plugins/target/python/python.go` -- 310 lines
- `internal/plugins/target/golang/golang.go` -- 323 lines
- `internal/plugins/target/java/` -- 5 files, ~240 lines (MISSING manifest + runner)

### Infrastructure (IMPLEMENTED BUT NOT WIRED)
- `internal/observability/tracing.go` -- OpenTelemetry (never initialized)
- `internal/observability/metrics.go` -- Prometheus metrics (never exposed)
- `internal/observability/audit.go` -- Structured audit logger (never initialized)
- `internal/server/health.go` -- Health endpoints (never started)
- `internal/server/shutdown.go` -- Graceful shutdown (never used)
- `internal/secrets/secrets.go` -- Secrets manager with Vault (never called)
- `internal/graph/neo4j/neo4j.go` -- Neo4j integration (never connected in CLI)
- `internal/vector/qdrant/qdrant.go` -- Qdrant integration (never connected in CLI)

### Critical Bugs
- `internal/temporal/activities.go:138,164` -- Silent `json.Marshal` error swallowing
- `internal/harness/dbdiff.go:156-161` -- SQL injection via `fmt.Sprintf` with table names
- `internal/server/shutdown.go:155-159` -- Shutdown hook errors silently swallowed
- `configs/anvil.yaml` -- `temperature`, `max_tokens`, `log.*` are dead config
