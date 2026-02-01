# Anvil - Landing Page & Demo Flow UI Prompt

## Context

Anvil is an open-source, AI-powered legacy code modernization platform. It converts COBOL mainframe applications to Java Spring Boot using a multi-agent pipeline. The product is developer-facing, targeting engineering leads and CTOs at enterprises with legacy mainframe debt.

**Tech stack for the landing page:** Next.js 15 (App Router), React 19, Tailwind CSS 4, Framer Motion, shadcn/ui. Deploy on Vercel.

**Brand tone:** Technical credibility, not enterprise fluff. Think Vercel/Linear/Supabase aesthetic - dark mode primary, monospace accents, crisp data visualization. The audience is skeptical senior engineers, not executives watching slideware.

---

## Page Structure

### 1. Hero Section

**Headline:** "Forge legacy into modern code."
**Subheadline:** "Anvil is an open-source multi-agent platform that reads your COBOL, understands the business logic, and writes production-grade Java Spring Boot."

**Visual:** An animated split-pane terminal visualization:
- Left side: COBOL source scrolling (use real snippets from CardDemo - credit card transaction processing)
- Right side: Generated Java Spring Boot code appearing in sync
- A thin animated pipeline bar between them showing the 4 agent icons (Cartographer -> Specular -> Architect -> Judge) with a pulse moving left to right

**CTA:** "Try the demo" (scrolls to interactive demo) | "View on GitHub" (external link)

### 2. Live Stats Banner (Proof Bar)

A horizontal strip with animated counters (count-up on scroll into view):

```
1,118 source files parsed | 272 modules analyzed | 1,064 functions extracted | 3,846 data types mapped | 380 Java files generated | <1s total processing
```

Small footnote: "Benchmarked across 7 open-source COBOL projects including AWS CardDemo, CobolCraft, and IBM samples."

### 3. How It Works - Pipeline Visualization

An interactive horizontal pipeline with 4 stages. Each stage is a card that expands on click/hover to show details.

**Stage 1: Cartographer**
- Icon: Map/compass
- Title: "Parse & Map"
- Description: "Reads COBOL source, builds a semantic graph of modules, functions, data types, call graphs, and I/O contracts. No LLM needed."
- Stat: "546 functions extracted from a single enterprise app"
- Visual: A miniature graph visualization (nodes = modules, edges = call relationships)

**Stage 2: Specular**
- Icon: Magnifying glass / prism
- Title: "Extract Business Rules"
- Description: "AI agent analyzes the semantic graph to identify and document business rules, validation logic, and domain patterns."
- Stat: "Understands EVALUATE/WHEN, PERFORM THRU, EXEC CICS, EXEC SQL"
- Visual: Highlighted code blocks showing rule extraction

**Stage 3: Architect**
- Icon: Blueprint / drafting compass
- Title: "Generate Modern Code"
- Description: "Transforms the enriched IR into idiomatic Java Spring Boot with proper type mapping, Spring annotations, and clean architecture."
- Stat: "270 Java files generated from CobolCraft in 462ms"
- Visual: Side-by-side before/after showing COBOL PIC 9(5) -> int, PIC X(30) -> String, COMP-3 -> BigDecimal

**Stage 4: Judge**
- Icon: Scale / checkmark
- Title: "Verify Equivalence"
- Description: "Validates that generated code preserves the original business semantics. Failed? Architect retries automatically (up to 3x)."
- Stat: "Automated retry loop ensures correctness"
- Visual: A small circular retry diagram

### 4. Interactive Demo Section

**Title:** "See it work. Right now."

A tabbed interface where users can pick from real COBOL examples and see the pipeline output:

**Tab 1: "Calculator" (Simple)**
- Show the COBOL source (calculator.cbl - ~15 lines)
- Click "Run Anvil" button
- Animated pipeline progress (each agent lights up sequentially)
- Output: The generated Calculator.java with syntax highlighting
- Metrics sidebar: 1 module, 2 functions, 3 data types, 2ms

**Tab 2: "CardDemo - Sign On" (Enterprise)**
- Show COSGN00C.cbl (AWS CardDemo sign-on screen - CICS program)
- Same pipeline animation
- Output: Generated Cosgn00c.java
- Metrics sidebar showing CICS detection, EXEC blocks, screen I/O

**Tab 3: "Full Project" (Scale)**
- Instead of showing code, show the aggregate metrics dashboard for all 7 projects
- An animated bar chart or table showing files/modules/functions/data types per project
- Processing time comparison (all under 500ms)

**Implementation note:** The demo does NOT call the real backend. Pre-compute all outputs and bundle them as static JSON. The animation is purely frontend - simulate the pipeline timing with delays.

### 5. Benchmark Results Section

**Title:** "Battle-tested on real mainframe code."

A data table (sortable, with hover highlights) showing the full benchmark:

| Project | Source | Files | Modules | Functions | Data Types | Call Edges | I/O Contracts | Java Files | Time |
|---------|--------|-------|---------|-----------|------------|------------|---------------|------------|------|
| AWS CardDemo | Credit card processing | 61 | 29 | 546 | 2,032 | 708 | 532 | 60 | 318ms |
| CobolCraft | Game server | 311 | 206 | 423 | 1,363 | 1,658 | 247 | 270 | 462ms |
| COBOL Library | AS/400 + OpenCobol | 126 | 28 | 54 | 226 | 50 | 114 | 30 | 17ms |
| COBOL Check | Unit test framework | 413 | 2 | 12 | 21 | 11 | 15 | 7 | 20ms |
| Programming Course | IBM Learning | 3 | 3 | 24 | 82 | 25 | 51 | 5 | 12ms |
| COBOL Unit Test | Test fixtures | 153 | 2 | 2 | 44 | 2 | 12 | 4 | 30ms |
| COBOL is Fun | IBM Developer | 51 | 2 | 3 | 78 | 0 | 35 | 4 | 4ms |

Below the table, a summary row:
**"1,118 files -> 380 Java classes in under 1 second. Zero API calls. Zero cloud dependencies."**

### 6. Architecture Section

**Title:** "Designed for extensibility."

A clean architecture diagram (use an SVG or animated diagram, NOT an image):

```
                    CLI / API
                       |
              Temporal Workflow
             /    |     |     \
     Cartographer  Specular  Architect  Judge
             \    |     |     /
              Semantic Graph (IR)
             /         |         \
        Neo4j       Qdrant     LLM Provider
      (Graph DB)  (Vector DB)  (Anthropic/OpenAI/
                                Ollama/Groq/...)
```

Three feature cards below:

**Pluggable LLM:** "Works with Claude, GPT-4, Groq, Ollama, vLLM, or no LLM at all. Template-only mode runs fully offline."

**Source/Target Plugins:** "COBOL -> Java today. The plugin interface supports any source language and any target. RPG, PL/I, Fortran - bring your own parser."

**Temporal Orchestration:** "Production-grade workflow engine with retry logic, observability, and distributed execution. Not a script."

### 7. Before/After Code Comparison

**Title:** "From mainframe to microservice."

A polished side-by-side diff view with syntax highlighting:

**Left (COBOL):**
```cobol
       IDENTIFICATION DIVISION.
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
           DISPLAY "SUM: " WS-RESULT.
```

**Right (Java Spring Boot):**
```java
@Service
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
}
```

Annotations connecting the two sides with thin lines:
- `PIC 9(5)` -> `int` (with tooltip: "5-digit numeric maps to Java int")
- `PIC 9(10)` -> `long` (with tooltip: "10-digit numeric maps to Java long")
- `PERFORM ADD-NUMBERS` -> `addNumbers()` (with tooltip: "PERFORM maps to method call")
- `WORKING-STORAGE` -> `private fields` (with tooltip: "Working storage becomes instance fields")

### 8. Feature Grid

**Title:** "What Anvil understands."

A 3x4 grid of capability cards with icons:

| COBOL Feature | Status | Detail |
|--------------|--------|--------|
| Fixed-format (cols 1-72) | Supported | Comment detection, Area A/B parsing |
| PERFORM THRU/UNTIL/VARYING | Supported | All loop variants mapped |
| EXEC CICS (SEND/RECEIVE) | Supported | Screen I/O detection |
| EXEC SQL (SELECT/INSERT/UPDATE/DELETE) | Supported | DB I/O classification |
| PIC 9/X/A/V/S clauses | Supported | Full type inference |
| COMP/COMP-3/BINARY/PACKED-DECIMAL | Supported | Usage clause mapping |
| 88-level conditions | Supported | Boolean constant detection |
| OCCURS (arrays) | Supported | Array type wrapping |
| COPY/REDEFINES | Supported | Copybook + overlay tracking |
| EVALUATE/WHEN | Supported | Complexity metrics |
| Sections & paragraphs | Supported | Both detected as functions |
| CALL 'external-program' | Supported | Cross-program dependency tracking |

### 9. Onboarding / Getting Started

**Title:** "Up and running in 60 seconds."

Three steps with terminal-style code blocks:

```bash
# Step 1: Install
go install github.com/efebarandurmaz/anvil/cmd/anvil@latest

# Step 2: Run (no config needed, works offline)
anvil run --source cobol --target java \
  --input ./your-cobol-src \
  --output ./generated-java

# Step 3: See what happened
anvil run --source cobol --target java \
  --input ./your-cobol-src \
  --output ./generated-java \
  --json | jq .
```

A note below: "Want AI-powered business rule extraction? Add your LLM provider:"

```yaml
# anvil.yaml
llm:
  provider: anthropic  # or openai, groq, ollama
  api_key: ${ANTHROPIC_API_KEY}
  model: claude-sonnet-4-20250514
```

### 10. Footer CTA

**Headline:** "Your mainframe code isn't going to modernize itself."

Two buttons:
- "Star on GitHub" (primary, links to repo)
- "Read the docs" (secondary, links to README)

Small text: "Anvil is open source under MIT license. Built with Go, Temporal, Neo4j, and Qdrant."

---

## Design Guidelines

### Colors (Dark Mode Primary)
- Background: `#09090b` (zinc-950)
- Surface: `#18181b` (zinc-900)
- Border: `#27272a` (zinc-800)
- Primary accent: `#f97316` (orange-500) - forge/anvil theme
- Secondary accent: `#3b82f6` (blue-500) - for code/tech elements
- Success: `#22c55e` (green-500) - for "supported" badges
- Text primary: `#fafafa` (zinc-50)
- Text secondary: `#a1a1aa` (zinc-400)
- Code background: `#0c0c0e`

### Typography
- Headings: Inter or Geist Sans, bold
- Body: Inter or Geist Sans, regular
- Code/monospace: Geist Mono or JetBrains Mono
- Stats/numbers: Tabular numerals, Geist Mono

### Animations
- Use Framer Motion for all scroll-triggered animations
- Pipeline animation: sequential reveal with 200ms stagger
- Counter animations: count-up over 1.5s with easing
- Code comparison: typewriter effect on first view
- Keep all animations subtle and fast - no bouncing, no excessive easing

### Responsive
- Desktop: Full pipeline horizontal layout
- Tablet: Pipeline stacks to 2x2 grid
- Mobile: Pipeline becomes vertical timeline

### Performance
- All demo data is static JSON (no API calls)
- Code blocks use static syntax highlighting (shiki or prism, not runtime)
- Images: SVG for diagrams, no raster images except OG meta
- Target: 100 Lighthouse performance score
