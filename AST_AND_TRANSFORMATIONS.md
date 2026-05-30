# VerCors AST and Transformation Architecture Explained

## 1. Does VerCors Use AST to Understand Program Flow?

**YES** - The entire system is built on AST manipulation.

### AST-Based Flow Understanding

```scala
// In Transformation.scala:
// The system walks through the AST tree recursively

override def run(input: Verification[_ <: Generation]): Verification[_ <: Generation] = {
  // For each pass in the chain:
  for ((passIndex, pass) <- passes.indices.zip(passes)) {
    
    // Execute the pass
    result = pass().dispatch(result)
    // ↑ This calls dispatch() which traverses the entire AST
    
    // Type-check the result
    // This requires understanding all AST nodes and their relationships
    result.tasks.map(_.program)
      .flatMap(program => program.check.map(program -> _))
  }
}
```

### How AST Traversal Works

Every rewriter follows this pattern:

```scala
// Example: DetectDeadCode (or any other rewriter)

case class MyRewriter[Pre <: Generation]() extends Rewriter[Pre] {
  
  // Entry point: traverse the whole program
  override def dispatch(program: Program[Pre]): Program[Post] = {
    // ... process program ...
    program.rewrite()  // ← Walks entire AST tree
  }
  
  // Process declarations
  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case proc: Procedure[Pre] =>
        // Process procedure specifics
        rewriteDefault(decl)  // ← Recursively processes children
      case _ => rewriteDefault(decl)
    }
  }
  
  // Process statements
  override def dispatch(stat: Statement[Pre]): Statement[Post] = {
    stat match {
      case ifStmt @ IfElse(cond, trueBranch, falseBranch) =>
        // Process condition
        val newCond = dispatch(cond)  // ← Recursive: process expression
        // Process branches
        val newTrue = dispatch(trueBranch)   // ← Recursive: process statement
        val newFalse = dispatch(falseBranch) // ← Recursive: process statement
        ifStmt.rewrite(cond = newCond, trueBranch = newTrue, falseBranch = newFalse)
      case _ => rewriteDefault(stat)
    }
  }
  
  // Process expressions
  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    e match {
      case binOp @ Plus(left, right) =>
        val newLeft = dispatch(left)    // ← Recursive
        val newRight = dispatch(right)  // ← Recursive
        binOp.rewrite(left = newLeft, right = newRight)
      case _ => rewriteDefault(e)
    }
  }
}
```

### Control Flow Analysis via AST

The AST structure allows understanding:

```
Program
  ├─ Declarations
  │   ├─ Class
  │   │   ├─ Field
  │   │   └─ Method
  │   │       └─ Contract
  │   │           ├─ Precondition (← For dead code detection!)
  │   │           ├─ Postcondition
  │   │           └─ Body: Statement*
  │   │
  │   └─ Function
  │       └─ Body: Expression
  │
  └─ Global Variables

Statement (Control Flow)
  ├─ IfElse(cond, thenBranch, elseBranch)    ← Branch structure
  ├─ While(cond, inv, body)                   ← Loop structure
  ├─ Block(statements)                        ← Sequential structure
  ├─ Return(value)                            ← Control flow change
  ├─ Break, Continue                          ← Loop control
  └─ TryCatch, Throw                          ← Exception control

Expression (Values & Conditions)
  ├─ Plus, Minus, Mult, Div                   ← Arithmetic
  ├─ And, Or, Not                             ← Logic
  ├─ Eq, Lt, Gt                               ← Comparison
  └─ InstanceFieldInvocation                  ← Data access
```

**How Dead Code Detection Uses AST:**

```scala
// We have access to:
1. Procedure contract preconditions
   ↓
2. Extract from AST: proc.contract.preconditions
   ↓
3. Walk to IfElse statements in body
   ↓
4. Extract condition: IfElse(cond, ...)
   ↓
5. Analyze: Is cond contradictory with preconditions?
   → Uses AST structure to understand relationships
```

---

## 2. Different Languages vs. Single Transformation

**KEY INSIGHT**: Transformations are **language-agnostic**, but there's a language-specific layer.

### Architecture: Two Layers

```
┌─────────────────────────────────────────────────────────────┐
│  Language-Specific Layer (Front-end)                         │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  LangJavaToCol    LangPVLToCol    LangCToCol    LangLLVMToCol│
│      │                 │              │              │       │
│      └─────────────────┴──────────────┴──────────────┘       │
│                        ↓                                      │
│              LangSpecificToCol (Common adapter)              │
│                        ↓                                      │
│  ┌────────────────────────────────────────────────────────┐  │
│  │ Unified Internal Representation (COL AST)              │  │
│  │                                                         │  │
│  │ Statement | Expression | Declaration | Type           │  │
│  │ (same for all languages)                               │  │
│  └────────────────────────────────────────────────────────┘  │
│                        ↓                                      │
├─────────────────────────────────────────────────────────────┤
│  Language-Agnostic Transformations (Rewrites)                │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  100+ Rewrite Passes (same for all languages)               │
│                                                              │
│  Disambiguate                                               │
│  EncodeBoundsChecks                                         │
│  DetectDeadCode            ← Applied to ALL languages      │
│  EncodeForkJoin                                             │
│  ClassToRef                                                 │
│  ... 95 more passes ...                                     │
│                                                              │
├─────────────────────────────────────────────────────────────┤
│  Output: Viper Program (Same format for all)                │
└─────────────────────────────────────────────────────────────┘
```

### Code Structure

**Location: `src/rewrite/vct/rewrite/lang/`**

```
lang/
├── LangSpecificToCol.scala      ← Base: Common interface
├── LangJavaToCol.scala          ← Java-specific adapter
├── LangPVLToCol.scala           ← PVL-specific adapter
├── LangCToCol.scala             ← C-specific adapter
├── LangLLVMToCol.scala          ← LLVM-specific adapter
├── LangVeyMontToCol.scala       ← VeyMont-specific adapter
├── LangSilverToCol.scala        ← Viper-specific adapter
├── LangBipToCol.scala           ← BIP-specific adapter
└── ... other utilities ...
```

### How Language-Specific Adapters Work

**Example: Java vs PVL**

```scala
// LangJavaToCol.scala
case class LangJavaToCol[Pre <: Generation](rw: LangSpecificToCol[Pre]) {
  // Handles Java-specific constructs:
  // - Classes with inheritance
  // - Null pointer semantics
  // - Type erasure
  // - Modifiers (public, private, synchronized)
  
  private def rewriteClass(cls: JavaClass[Pre]): Unit = {
    // Convert Java class to COL equivalent
    // Handle inheritance, null checks, etc.
  }
}

// LangPVLToCol.scala
case class LangPVLToCol[Pre <: Generation](rw: LangSpecificToCol[Pre]) {
  // Handles PVL-specific constructs:
  // - Permissions (\pointsto, 1/2)
  // - Predicates (with contracts)
  // - History assertions
  // - Choreography constructs
  
  private def rewritePredicate(pred: PVLPredicate[Pre]): Unit = {
    // Convert PVL predicate to COL equivalent
    // Handle permissions, dependencies, etc.
  }
}
```

**Common Output (COL AST):**

```scala
// Both adapt to the same COL types:

// Java input: public int getValue() { return value; }
// PVL input: int getValue() = value;
//
// Both become: Function[COL](args, returnType, body, contract)

// Java input: if (x == null) { ... }
// PVL input: if (x == null) { ... }
//
// Both become: IfElse[COL](cond, thenBranch, elseBranch)
```

---

## 3. Smoke Tests: One Place vs. Multiple Places?

### Answer: **Mostly ONE place, but with language variations**

### Architecture

```
examples/smoke_tests/
├── [General tests - applied to ALL languages]
│   ├── dead_code_pointer_precond.java
│   ├── dead_code_pvl.pvl
│   └── ...
│
├── [Language-specific tests]
│   ├── java/
│   │   └── [Java-only features]
│   │
│   ├── pvl/
│   │   └── [PVL-only features]
│   │
│   └── c/
│       └── [C-specific features]
│
└── [Currently mixed]
    ├── dead-code.java
    ├── dead-code.pvl
    ├── test_abstract.pvl
    ├── test.java
    └── ...
```

### Where to Put Smoke Tests

**For a general transformation (applies to all languages):**

```
✅ Single test file in examples/smoke_tests/
   dead_code_general.java    (Java example)
   dead_code_general.pvl     (PVL example)
   dead_code_general.c       (C example)
   
   All test the SAME transformation
   But in different language syntaxes
```

**For a language-specific transformation:**

```
✅ Language-specific subfolder
   examples/smoke_tests/java/        (Java features)
   examples/smoke_tests/pvl/         (PVL features)
   examples/smoke_tests/c/           (C features)
```

### Example: DetectDeadCode Smoke Tests

For **DetectDeadCode** rewriter, we need ONE transformation that works on all languages:

```
✅ Current approach (CORRECT):
   examples/smoke_tests/dead_code_pointer_precond.java
   examples/smoke_tests/dead_code_pvl.pvl
   
   Both test SAME rewriter (DetectDeadCode)
   In different language syntaxes
   
   ✓ No separate transformation needed
   ✓ Rewriter applies to both
   ✓ Tests verify it works for all languages

✅ Why separate files?
   Because Java syntax ≠ PVL syntax
   But the AST transformation is identical
```

---

## 4. How Transformations Are Applied to All Languages

### The Pipeline

```
Input File (Java/PVL/C)
    ↓
Parse to Language-Specific AST
    ↓
Convert to Common COL AST (via LangXxxToCol)
    ↓ ← All languages unified here
    ↓
Pass Chain: DetectDeadCode (applies to ALL)
    ↓ (same transformation, same AST types)
Pass Chain: EncodeForkJoin (applies to ALL)
    ↓ (same transformation, same AST types)
...100+ more passes...
    ↓
Convert COL to Viper
    ↓
Verification Backend (Silicon/Carbon)
```

### Code Example: Single Transformation, Multiple Languages

```scala
// DetectDeadCode.scala (NO language-specific code needed!)

case class DetectDeadCode[Pre <: Generation]() extends Rewriter[Pre] {
  
  override def dispatch(stmt: Statement[Pre]): Statement[Post] = {
    stmt match {
      case IfElse(cond, tBranch, fBranch) =>
        // This works for:
        // - Java IfElse statements
        // - PVL IfElse statements
        // - C IfElse statements
        // - All convert to same COL IfElse type!
        
        checkConditionFeasibility(cond) match {
          case AlwaysFalse =>
            logger.warn("Dead code detected")  // Same warning for all!
          case _ =>
            // normal processing
        }
      case While(cond, inv, body) =>
        // Works for all languages too!
        if (isAlwaysFalse(cond)) {
          logger.warn("Loop never executes")
        }
    }
  }
}
```

---

## 5. Summary Table

| Aspect | Details |
|--------|---------|
| **AST Usage** | ✅ Yes, entire system is AST-based via traversal |
| **Control Flow** | Via AST structure (IfElse, While, Block, Return) |
| **Dead Code Detection** | Analyzes AST preconditions vs. branch conditions |
| **Transformation Scope** | Applies to ALL nodes matching the pattern |
| **Language Separation** | Front-end only (LangXxxToCol adapters) |
| **Transformation Reuse** | 100% - same code for all languages |
| **Smoke Tests** | Separate test files per language, same transformation |
| **Test Location** | `examples/smoke_tests/` (no subdirs needed for general transforms) |
| **Test Structure** | Write in each language syntax, tests same rewriter |

---

## 6. Practical Example: Adding DetectDeadCode Smoke Tests

```
Create:
  examples/smoke_tests/dead_code_pointer_precond.java
  examples/smoke_tests/dead_code_pvl.pvl
  examples/smoke_tests/dead_code_general.c (optional)

All test SAME rewriter (DetectDeadCode)
No separate transformations needed!

Each file:
  //:: cases TestDeadCodePointerPrecond
  //:: tools silicon
  //:: verdict Pass
  
  (Language-specific code)

Pipeline:
  Java file → LangJavaToCol → COL AST → DetectDeadCode ✓
  PVL file  → LangPVLToCol  → COL AST → DetectDeadCode ✓
  C file    → LangCToCol    → COL AST → DetectDeadCode ✓
```

---

## 7. Why This Design?

**Benefits:**

1. **Language Independence**: Add new language → old transforms still work
2. **Code Reuse**: Write transformation once → works for all languages
3. **Consistency**: Same semantics across Java/PVL/C
4. **Maintainability**: Change logic in one place
5. **Scalability**: 100+ transforms × 1 implementation = scalable

**Example**: If we didn't have this layer:

```
BAD (100+ × number of languages):
  Disambiguate_Java.scala
  Disambiguate_PVL.scala
  Disambiguate_C.scala
  Disambiguate_LLVM.scala
  EncodeForkJoin_Java.scala
  EncodeForkJoin_PVL.scala
  ...
  = 400+ files with duplicated logic

GOOD (what VerCors does):
  Disambiguate.scala (applies to all via COL)
  EncodeForkJoin.scala (applies to all via COL)
  ...
  = 100 files with shared logic
```

---

**Key Takeaway**: 

- ✅ System uses AST trees for control flow understanding
- ✅ Transformations are language-agnostic (work on COL AST)
- ✅ Write smoke tests in different languages, same transformation
- ✅ No need for separate transformation implementations per language
- ✅ Front-end adapters (LangXxxToCol) handle language differences
