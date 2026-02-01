# COBOL Parser Requirements Analysis
## Enterprise CardDemo Application Analysis

Date: 2026-02-02
Source: /Users/efebarandurmaz/anvil/testdata/carddemo/app/cbl/

---

## Executive Summary

This analysis examines 6 diverse COBOL programs from a real-world enterprise card processing system to identify parsing patterns and requirements. The programs range from 41 to 4,236 lines and represent:

- **Batch processing** (file exports, interest calculation, statement generation)
- **Online transaction processing** (CICS programs for card management)
- **Utility programs** (wait/sleep functions)

Key finding: **Enterprise COBOL parsers must handle both modern structured patterns AND legacy constructs** (GO TO, ALTER, self-modifying code) that still appear in production systems.

---

## Files Analyzed

| File | Size (LOC) | Type | Key Features |
|------|------------|------|--------------|
| CBEXPORT.cbl | 582 | Batch Export | Clean structured code, indexed files |
| CBACT04C.cbl | 652 | Batch Interest Calculator | Section-based, COMP variables |
| COBSWAIT.cbl | 41 | Utility | Minimal program, ACCEPT/CALL |
| CBSTM03A.CBL | 924 | Statement Generator | ALTER/GO TO, 2D arrays, control blocks |
| COCRDLIC.cbl | 1,459 | CICS Card List | EXEC CICS, 88-levels, COMMAREA |
| COACTUPC.cbl | 4,236 | CICS Card Update | Large CICS program, extensive validation |

---

## Detailed Analysis by File

### 1. CBEXPORT.cbl - Batch Data Export Program

**Type:** Batch COBOL
**Lines:** 582
**Purpose:** Export customer data from indexed files to sequential export file

#### Structure
- **Paragraphs:** 21 (primary organization method)
- **Sections:** 3 (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- **Control Flow:** Clean structured programming with named PERFORMs

#### Key Parsing Patterns

**PROCEDURE DIVISION Structure:**
```cobol
PROCEDURE DIVISION.
0000-MAIN-PROCESSING.
    PERFORM 1000-INITIALIZE
    PERFORM 2000-EXPORT-CUSTOMERS
    PERFORM 3000-EXPORT-ACCOUNTS
    PERFORM 4000-EXPORT-XREFS
    PERFORM 5000-EXPORT-TRANSACTIONS
    PERFORM 5500-EXPORT-CARDS
    PERFORM 6000-FINALIZE
    GOBACK.
```

**Features Used:**
- **COPY statements:** 6 copybooks for record layouts
- **File I/O:** 5 READ, 5 WRITE operations
- **88-level conditions:** `88 WS-CUSTOMER-EOF VALUE '10'`
- **STRING operations:** 3 instances for timestamp formatting
- **PERFORM UNTIL:** Sequential file processing pattern
- **INDEXED files:** With RECORD KEY, ALTERNATE KEY, FILE STATUS

**Unusual Patterns:**
- Multiple indexed input files (5) with different organizations
- Error handling via PERFORM 9999-ABEND-PROGRAM calling CEE3ABD
- ACCEPT FROM DATE/TIME for timestamps

---

### 2. CBACT04C.cbl - Interest Calculator

**Type:** Batch COBOL
**Lines:** 652
**Purpose:** Calculate interest on account balances, update accounts

#### Structure
- **Paragraphs:** 0 (all code in sections)
- **Sections:** 4 divisions (standard structure)
- **Control Flow:** Section-based with numbered prefixes (0000-, 1000-, 9000-)

#### Key Parsing Patterns

**PROCEDURE DIVISION with USING:**
```cobol
PROCEDURE DIVISION USING EXTERNAL-PARMS.
```

**Features Used:**
- **COPY statements:** 5 copybooks
- **COMP variables:** Binary integers for efficiency
- **LINKAGE SECTION:** For parameter passing
- **File Access Modes:** Mix of SEQUENTIAL and RANDOM
- **ALTERNATE RECORD KEY:** Secondary indexes
- **READ with KEY:** Direct access by alternate key
- **REWRITE:** In-place file updates
- **COMPUTE:** For interest calculation

**Notable Patterns:**
```cobol
READ XREF-FILE INTO CARD-XREF-RECORD
    KEY IS FD-XREF-ACCT-ID
    INVALID KEY
        DISPLAY 'ACCOUNT NOT FOUND: ' FD-XREF-ACCT-ID
END-READ
```

**Unusual Patterns:**
- Timestamp formatting with REDEFINES for DB2-compatible format
- Complex FILE STATUS checking (numeric vs alphanumeric)
- Multi-file coordinated updates (read from 4 files, write to 2)

---

### 3. COBSWAIT.cbl - Wait Utility

**Type:** Utility
**Lines:** 41
**Purpose:** Simple wait/sleep program for batch job timing

#### Structure
- **Paragraphs:** 0
- **Sections:** 2 (minimal divisions)
- **Control Flow:** Linear - no loops or branches

#### Key Parsing Patterns

**Simplest Valid COBOL Program:**
```cobol
PROCEDURE DIVISION.
    ACCEPT PARM-VALUE FROM SYSIN.
    MOVE PARM-VALUE TO MVSWAIT-TIME.
    CALL 'MVSWAIT' USING MVSWAIT-TIME.
    STOP RUN.
```

**Features Used:**
- **ACCEPT FROM SYSIN:** JCL parameter input
- **CALL USING:** External program invocation
- **COMP (BINARY):** For numeric parameter passing
- **STOP RUN:** Program termination (vs GOBACK)

**Parser Requirement:** Must handle minimal programs with no WORKING-STORAGE complexity

---

### 4. CBSTM03A.CBL - Statement Generator

**Type:** Batch with Advanced Features
**Lines:** 924
**Purpose:** Generate customer statements in text and HTML formats

#### Structure
- **Paragraphs:** 26
- **Sections:** 4
- **Control Flow:** **LEGACY - Uses ALTER and GO TO extensively**

#### Key Parsing Patterns

**SELF-MODIFYING CODE:**
```cobol
EVALUATE WS-FL-DD
  WHEN 'TRNXFILE'
    ALTER 8100-FILE-OPEN TO PROCEED TO 8100-TRNXFILE-OPEN
    GO TO 8100-FILE-OPEN
  WHEN 'XREFFILE'
    ALTER 8100-FILE-OPEN TO PROCEED TO 8200-XREFFILE-OPEN
    GO TO 8100-FILE-OPEN
```

**Features Used:**
- **ALTER:** 4 instances (self-modifying flow control)
- **GO TO:** 15 instances (legacy branching)
- **EVALUATE/WHEN:** 5 instances (modern case statements)
- **PERFORM THRU:** 2 instances with EXIT paragraphs
- **CALL:** 14 subroutine calls to CBSTM03B
- **COMP-3 (PACKED):** For decimal calculations
- **2D OCCURS:** `WS-TRAN-TBL OCCURS 10 TIMES` nested in `WS-CARD-TBL OCCURS 51 TIMES`
- **STRING with DELIMITED:** Complex string building for HTML
- **88-level with multiple VALUES:** Condition names

**LINKAGE to Control Blocks (MVS-specific):**
```cobol
LINKAGE SECTION.
01  PSA-BLOCK.
    05  FILLER       PIC X(536).
    05  TCB-POINT    POINTER.
01  TCB-BLOCK.
    05  FILLER       PIC X(12).
    05  TIOT-POINT   POINTER.
```

**Unusual Patterns:**
- SET ADDRESS OF for MVS control block access
- POINTER data types
- REDEFINES with OCCURS for table manipulation
- SET condition-name TO TRUE for 88-level assignment
- Recursive GO TO (GO TO 8500-READTRNX-READ within same paragraph)

---

### 5. COCRDLIC.cbl - CICS Card List

**Type:** CICS Online Program
**Lines:** 1,459
**Purpose:** Display list of credit cards with paging

#### Structure
- **Paragraphs:** 0 (all code in sections)
- **Sections:** 3
- **Control Flow:** Event-driven (CICS pseudo-conversational)

#### Key Parsing Patterns

**EXEC CICS Commands (18 occurrences):**
```cobol
EXEC CICS XCTL
    PROGRAM (LIT-MENUPGM)
    COMMAREA(CARDDEMO-COMMAREA)
END-EXEC

EXEC CICS RECEIVE MAP(LIT-THISMAP)
    MAPSET(LIT-THISMAPSET)
    INTO(CCRDLIAI)
END-EXEC

EXEC CICS SEND MAP(LIT-THISMAP)
    MAPSET(LIT-THISMAPSET)
    FROM(CCRDLIAO)
    CURSOR
END-EXEC
```

**Features Used:**
- **EXEC CICS:** 18 commands (SEND, RECEIVE, XCTL, RETURN, etc.)
- **EVALUATE TRUE:** 18 instances for complex decision logic
- **COPY DFHBMSCA/DFHAID:** IBM-supplied CICS copybooks
- **COMMAREA:** EIBCALEN, DFHCOMMAREA for pseudo-conversational state
- **88-levels extensively:** `88 CCARD-AID-ENTER VALUE DFHENTER`
- **OCCURS DEPENDING ON:** `OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN`
- **COMP:** For screen subscripts and counters
- **GO TO:** 16 instances (common in CICS programs for error handling)

**LINKAGE SECTION for CICS:**
```cobol
LINKAGE SECTION.
01  DFHCOMMAREA.
  05  FILLER  PIC X(1)
      OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.
```

**Unusual Patterns:**
- EIB fields (Execute Interface Block): EIBCALEN, EIBAID
- Map-based I/O with symbolic description maps
- Pseudo-conversational design (save state in COMMAREA)
- Attribute bytes for screen field protection
- Complex pagination logic with forward/backward reads

---

### 6. COACTUPC.cbl - CICS Account Update

**Type:** CICS Online Program
**Lines:** 4,236 (largest file analyzed)
**Purpose:** Update account information via online screens

#### Structure
- **Paragraphs:** 88 (extensive modular design)
- **Sections:** 3
- **Control Flow:** Complex with 51 GO TO statements

#### Key Parsing Patterns

**EXEC CICS with HANDLE ABEND:**
```cobol
EXEC CICS HANDLE ABEND
    LABEL(ABEND-HANDLING)
END-EXEC
```

**Features Used:**
- **EXEC CICS:** 17 commands
- **EVALUATE/WHEN:** 10 instances
- **COPY:** 56 copybooks (extensive reuse)
- **GO TO:** 51 instances (complex branching for validation)
- **PERFORM THRU:** 2 instances
- **STRING:** 7 operations for message construction
- **COMP-3:** Packed decimal for financial amounts
- **Complex validation:** Field-level edit flags with 88-levels

**Screen Handling:**
- Attribute byte manipulation (FSET, FRSET)
- Cursor positioning
- Message area management
- Field-level error highlighting

**Unusual Patterns:**
- Large copybook usage (56 files)
- Deep nesting of IF/EVALUATE/PERFORM
- Extensive use of symbolic constants (LIT-xxx)
- REDEFINES for type conversion (X to 9)
- Multiple program XCTL chains

---

## Summary: Parser Requirements for Enterprise COBOL

### 1. **Division and Section Handling**

**Must Support:**
- 4 standard divisions: IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE
- Optional sections: FILE-CONTROL, INPUT-OUTPUT, WORKING-STORAGE, LINKAGE
- Section declarations: `paragraph-name SECTION.`
- Flexible ordering (some programs have minimal ENVIRONMENT DIVISION)

**Variations Found:**
- Programs with 0 paragraphs (section-based only)
- Programs with 88 paragraphs (highly modular)
- Mixed paragraph/section styles in same program

---

### 2. **PROCEDURE DIVISION Structures**

#### Paragraph-Based (CBEXPORT, COACTUPC, CBSTM03A)
```cobol
PROCEDURE DIVISION.
0000-MAIN-PROCESSING.
    PERFORM 1000-INITIALIZE
    PERFORM 2000-PROCESS
    GOBACK.

1000-INITIALIZE.
    [code]
    EXIT.
```

#### Section-Based (CBACT04C, COCRDLIC, COBSWAIT)
```cobol
PROCEDURE DIVISION.
    [inline code]
    GOBACK.
```

#### With Parameters
```cobol
PROCEDURE DIVISION USING EXTERNAL-PARMS.
```

**Parser Must Handle:**
- Numbered paragraph names (0000-, 1000-, 9000-)
- Named paragraph names (ABEND-HANDLER)
- Paragraphs with/without explicit EXIT
- Inline code before first paragraph
- GOBACK vs STOP RUN termination

---

### 3. **COPY Statement Variations**

**Basic:**
```cobol
COPY CVACT01Y.
```

**No variations found with REPLACING, but parser should support standard COPY syntax**

**Count by Program:**
- 0 (COBSWAIT) to 56 (COACTUPC)
- Critical for enterprise code reuse

---

### 4. **File I/O Patterns**

**File Definition (FILE-CONTROL):**
```cobol
SELECT CUSTOMER-INPUT ASSIGN TO CUSTFILE
    ORGANIZATION IS INDEXED
    ACCESS MODE IS SEQUENTIAL
    RECORD KEY IS CUST-ID
    ALTERNATE RECORD KEY IS CUST-SSN
    FILE STATUS IS WS-CUSTOMER-STATUS.
```

**Organizations:**
- INDEXED (most common)
- SEQUENTIAL
- RELATIVE (not seen but standard)

**Access Modes:**
- SEQUENTIAL
- RANDOM
- DYNAMIC (not seen but standard)

**I/O Verbs:**
- OPEN INPUT/OUTPUT/I-O
- READ with/without INTO
- READ with KEY IS
- READ with INVALID KEY
- WRITE with FROM
- REWRITE
- CLOSE

---

### 5. **CICS Embedded Commands**

**Must Parse:** `EXEC CICS ... END-EXEC` blocks

**Commands Found:**
- SEND MAP
- RECEIVE MAP
- XCTL (transfer control)
- RETURN
- HANDLE ABEND

**Parser Requirement:**
- Treat EXEC...END-EXEC as opaque statement blocks
- Preserve internal structure for CICS preprocessor
- Handle multi-line EXEC statements

---

### 6. **EVALUATE/WHEN (Case Statements)**

**Basic:**
```cobol
EVALUATE WS-STATUS-CODE
  WHEN '00'
    PERFORM SUCCESS-ROUTINE
  WHEN '10'
    PERFORM EOF-ROUTINE
  WHEN OTHER
    PERFORM ERROR-ROUTINE
END-EVALUATE
```

**Complex (EVALUATE TRUE):**
```cobol
EVALUATE TRUE
  WHEN INPUT-ERROR
    PERFORM ERROR-HANDLING
  WHEN CCARD-AID-PFK03 AND CDEMO-FROM-PROGRAM EQUAL LIT-THISPGM
    PERFORM EXIT-ROUTINE
  WHEN CCARD-AID-PFK07
    PERFORM PAGE-UP
END-EVALUATE
```

**Parser Must Handle:**
- Simple value evaluation
- EVALUATE TRUE with complex conditions
- WHEN with AND/OR logic
- WHEN OTHER (default case)
- Nested EVALUATE

---

### 7. **PERFORM Variations**

**Simple:**
```cobol
PERFORM 1000-INITIALIZE
```

**PERFORM THRU:**
```cobol
PERFORM 1000-INITIALIZE
   THRU 1000-INITIALIZE-EXIT
```

**PERFORM UNTIL:**
```cobol
PERFORM UNTIL END-OF-FILE = 'Y'
    READ CUSTOMER-FILE
    PERFORM PROCESS-RECORD
END-PERFORM
```

**PERFORM VARYING:**
```cobol
PERFORM VARYING CR-JMP FROM 1 BY 1
  UNTIL CR-JMP > CR-CNT
    [loop body]
END-PERFORM
```

**Inline vs Out-of-line:**
- Programs mix both styles
- Must handle scope terminators (END-PERFORM)

---

### 8. **Data Types and COMP Variations**

**COMP (Binary):**
```cobol
05  WS-RESP-CD  PIC S9(09) COMP.
05  I           PIC S9(4) COMP.
```

**COMP-3 (Packed Decimal):**
```cobol
05  WS-TOTAL-AMT  PIC S9(9)V99 COMP-3.
```

**USAGE Clause:**
```cobol
05  WS-EDIT-SELECT-COUNTER  PIC S9(04) USAGE COMP-3.
```

**Parser Must Support:**
- COMP / COMPUTATIONAL / BINARY (all synonyms)
- COMP-3 / PACKED-DECIMAL
- Implicit DISPLAY (default)
- USAGE before or after PIC

---

### 9. **88-Level Condition Names**

**Single Value:**
```cobol
05  WS-CUSTOMER-STATUS  PIC X(02).
    88  WS-CUSTOMER-EOF  VALUE '10'.
```

**Multiple Values:**
```cobol
88  INPUT-OK  VALUES '0' ' ' LOW-VALUES.
88  SELECT-OK  VALUES 'S', 'U'.
```

**Ranges:**
```cobol
88  DETAIL-WAS-REQUESTED  VALUES 1 THRU 7.
```

**SET usage:**
```cobol
SET WS-ERROR-MSG-OFF TO TRUE.
```

---

### 10. **OCCURS and Arrays**

**Simple OCCURS:**
```cobol
05  WS-EDIT-SELECT  PIC X(1) OCCURS 7 TIMES.
```

**2D Arrays (Nested OCCURS):**
```cobol
01  WS-TRNX-TABLE.
    05  WS-CARD-TBL OCCURS 51 TIMES.
        10  WS-CARD-NUM  PIC X(16).
        10  WS-TRAN-TBL OCCURS 10 TIMES.
            15  WS-TRAN-NUM  PIC X(16).
```

**OCCURS DEPENDING ON:**
```cobol
OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.
```

**Subscripting:**
```cobol
MOVE WS-TRAN-NUM (CR-JMP, TR-JMP) TO TRNX-ID
```

---

### 11. **REDEFINES**

**Type Conversion:**
```cobol
10  CARD-ACCT-ID-X  PIC X(11).
10  CARD-ACCT-ID-N REDEFINES CARD-ACCT-ID-X PIC 9(11).
```

**Union Types:**
```cobol
05  WS-EDIT-SELECT-FLAGS  PIC X(7).
05  WS-EDIT-SELECT-ARRAY REDEFINES WS-EDIT-SELECT-FLAGS.
   10  WS-EDIT-SELECT  PIC X(1) OCCURS 7 TIMES.
```

---

### 12. **STRING Operations**

**STRING with DELIMITED:**
```cobol
STRING CUST-FIRST-NAME DELIMITED BY ' '
       ' ' DELIMITED BY SIZE
       CUST-LAST-NAME DELIMITED BY ' '
       INTO ST-NAME
END-STRING
```

**Must Handle:**
- Multiple source fields
- DELIMITED BY literal/variable/SIZE
- INTO target
- Optional POINTER/OVERFLOW

---

### 13. **Legacy Constructs (Still in Production!)**

#### ALTER (Self-Modifying Code)
```cobol
ALTER 8100-FILE-OPEN TO PROCEED TO 8100-TRNXFILE-OPEN
```

**Found in:** CBSTM03A (4 instances)
**Parser Requirement:** Must parse but can flag as deprecated

#### GO TO
```cobol
GO TO 1000-MAINLINE.
```

**Found in:** 5 of 6 programs (0-51 instances)
**Parser Requirement:** Essential for real-world code

#### Fallthrough Paragraphs
```cobol
8100-FILE-OPEN.
    GO TO 8100-TRNXFILE-OPEN.

8100-TRNXFILE-OPEN.
    [code]
```

---

### 14. **LINKAGE SECTION and POINTERs**

**Parameter Passing:**
```cobol
LINKAGE SECTION.
01  EXTERNAL-PARMS.
    05  PARM-LENGTH  PIC S9(04) COMP.
    05  PARM-DATE    PIC X(10).
```

**CICS COMMAREA:**
```cobol
LINKAGE SECTION.
01  DFHCOMMAREA.
  05  FILLER  PIC X(1)
      OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.
```

**POINTERs (MVS Control Blocks):**
```cobol
01  PSAPTR  POINTER.
01  TCB-POINT  POINTER.

SET ADDRESS OF PSA-BLOCK TO PSAPTR.
```

---

### 15. **Special Statements**

**ACCEPT:**
```cobol
ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
ACCEPT WS-CURRENT-TIME FROM TIME
ACCEPT PARM-VALUE FROM SYSIN
```

**CALL:**
```cobol
CALL 'CEE3ABD' USING ABCODE, TIMING.
CALL 'CBSTM03B' USING WS-M03B-AREA.
```

**INITIALIZE:**
```cobol
INITIALIZE CC-WORK-AREA
           WS-MISC-STORAGE
           WS-COMMAREA
```

**COMPUTE:**
```cobol
COMPUTE WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200
```

**MOVE CORRESPONDING:**
Not found in sample, but common in enterprise code

---

## Critical Parser Features Summary

### Absolutely Required

1. **Four COBOL Divisions** - with flexible section ordering
2. **PROCEDURE DIVISION** - both paragraph and section-based
3. **COPY statements** - primary code reuse mechanism
4. **PERFORM variants** - simple, THRU, UNTIL, VARYING
5. **EVALUATE/WHEN** - modern case statements
6. **88-level conditions** - with VALUES, ranges, SET
7. **OCCURS** - single and nested arrays
8. **REDEFINES** - type punning and unions
9. **File I/O** - OPEN, READ, WRITE, CLOSE with INDEXED/SEQUENTIAL
10. **COMP/COMP-3** - binary and packed decimal

### Essential for CICS Programs

11. **EXEC CICS...END-EXEC** - embedded preprocessor commands
12. **LINKAGE SECTION** - with DFHCOMMAREA and EIBCALEN
13. **OCCURS DEPENDING ON** - variable-length tables
14. **EIB fields** - Execute Interface Block references

### Essential for Legacy Code

15. **GO TO** - still heavily used
16. **ALTER** - self-modifying code (rare but present)
17. **PERFORM THRU** - with explicit EXIT paragraphs
18. **Fallthrough paragraphs** - implicit flow control

### Advanced Features

19. **POINTER** - for control block addressing
20. **STRING/UNSTRING** - with DELIMITED BY
21. **ACCEPT FROM DATE/TIME** - system values
22. **CALL** - external program invocation
23. **INVALID KEY** - file error handling
24. **Inline PERFORM** - with END-PERFORM scope terminator

---

## Recommendations for Parser Implementation

### Phase 1: Core Language
- Standard 4 divisions
- Basic data types (PIC X, PIC 9, COMP, COMP-3)
- MOVE, PERFORM, IF/ELSE/END-IF
- Simple file I/O (OPEN, READ, WRITE, CLOSE)

### Phase 2: Enterprise Features
- COPY statements
- EVALUATE/WHEN
- 88-level conditions
- OCCURS and subscripting
- REDEFINES
- CALL

### Phase 3: CICS Support
- EXEC CICS...END-EXEC as opaque blocks
- LINKAGE SECTION with OCCURS DEPENDING ON
- DFHCOMMAREA pattern recognition

### Phase 4: Legacy Support
- GO TO and ALTER (with deprecation warnings)
- PERFORM THRU
- Fallthrough paragraph detection

### Phase 5: Advanced
- POINTER data types
- STRING/UNSTRING
- Complex OCCURS DEPENDING ON
- Nested COPY with REPLACING

---

## Test Cases Recommended

1. **Minimal Program** - COBSWAIT.cbl (41 lines)
2. **Clean Structured** - CBEXPORT.cbl (no GO TO, modern style)
3. **Section-Based** - CBACT04C.cbl (no standalone paragraphs)
4. **Legacy Code** - CBSTM03A.CBL (ALTER, GO TO, 2D arrays)
5. **CICS Online** - COCRDLIC.cbl (EXEC CICS, COMMAREA, maps)
6. **Large Complex** - COACTUPC.cbl (4,236 lines, 88 paragraphs)

---

## Conclusion

Enterprise COBOL code exhibits extreme diversity:

- **Modern structured code** exists alongside **1970s-style GO TO spaghetti**
- **Simple batch programs** (41 lines) to **complex online systems** (4,236 lines)
- **Clean paragraph-based** vs **section-only** organization
- **Standard COBOL** vs **CICS extensions** vs **MVS control blocks**

A production-grade parser must handle ALL these patterns, as they coexist in real enterprise systems. The CardDemo application demonstrates that even modern AWS-maintained code retains legacy constructs like ALTER and extensive GO TO usage.

**Key Insight:** Parser cannot assume "clean" code. Must support full COBOL-85 standard plus common vendor extensions (CICS, DB2) and legacy constructs still actively maintained in 2025+.
