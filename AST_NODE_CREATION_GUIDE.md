# VerCors AST Node Creation Guide: Creating and Inserting Assert Statements

This guide explains how to create and insert AST nodes (particularly Assert statements) in VerCors rewriter transformations.

## Table of Contents
1. [Assert Node Creation](#assert-node-creation)
2. [Blame Objects](#blame-objects)
3. [Inserting Statements into Blocks](#inserting-statements-into-blocks)
4. [Complete Examples](#complete-examples)
5. [Helper Functions](#helper-functions)

---

## Assert Node Creation

### Basic Assert Signature

The `Assert` statement is defined in `src/col/vct/col/ast/Node.scala` as:

```scala
final case class Assert[G](res: Expr[G])(val blame: Blame[AssertFailed])(
    implicit val o: Origin
) extends NormallyCompletingStatement[G]
```

### Assert Creation Pattern

There are three forms of creating Assert statements:

#### Form 1: With implicit Origin (most common in rewriters)
```scala
Assert(expression)(blame)
// The Origin is implicitly taken from the current scope
```

#### Form 2: With explicit Origin
```scala
implicit val o: Origin = someOrigin
Assert(expression)(blame)
```

#### Form 3: With explicit Origin in constructor
```scala
Assert(expression)(blame)(someOrigin)
```

### Complete Example: Basic Assert Creation

```scala
import vct.col.ast._
import vct.col.origin._

// Create a simple boolean assertion that 'x' is not null
implicit val origin: Origin = someNode.o  // Get origin from existing node

val assertNotNull = Assert(x !== Null())(
  PanicBlame("Object should not be null")
)
```

---

## Blame Objects

### What is Blame?

`Blame` is a trait that defines how verification errors should be reported. It's parametrized by the type of failure it can blame (e.g., `AssertFailed`, `ExhaleFailed`).

### Blame Trait Definition

```scala
// From src/col/vct/col/origin/Blame.scala
sealed trait Blame[E] {
  def blame(error: E): Unit
}
```

### Built-in Blame Objects

#### PanicBlame
Used when an error indicates an internal bug (shouldn't happen in valid code):
```scala
PanicBlame("Lock cannot fail after holding the lock")
```

#### Custom Blame Objects
Create case classes extending `Blame[AssertFailed]`:

```scala
// Pattern from EncodeIntrinsicLock.scala
case class NotCommittedAssertFailed(lock: Lock[_]) 
    extends Blame[AssertFailed] {
  override def blame(error: AssertFailed): Unit =
    lock.blame.blame(LockNotCommitted(lock))
}

// Pattern from InlineApplicables.scala
case class InlineFoldAssertFailed(fold: Fold[_]) 
    extends Blame[AssertFailed] {
  override def blame(error: AssertFailed): Unit =
    fold.blame.blame(FoldFailed(error.failure, fold))
}

// Pattern from RefuteToInvertedAssert.scala
case class FilterExpectedErrorBlame(
    defaultBlame: Blame[AssertFailed],
    err: ExpectedError
) extends Blame[AssertFailed] {
  override def blame(error: AssertFailed): Unit =
    // Custom error handling logic
}
```

### Key Points about Blame

1. **Chains Failures**: Blame objects typically chain to other blame objects, creating a hierarchy
2. **Maps Errors**: They translate verification errors into domain-specific error messages
3. **Preserves Context**: They maintain information about the original error source
4. **Scoped to Node**: Each blame object typically holds a reference to the node that caused the error

---

## Inserting Statements into Blocks

### Block Structure

```scala
// From src/col/vct/col/ast/Node.scala
final case class Block[G](statements: Seq[Statement[G]])(
    implicit val o: Origin
) extends NormallyCompletingStatement[G]
```

### Creating Blocks with Multiple Statements

```scala
// Pattern: Block(Seq(...statements...))

implicit val o: Origin = existingNode.o

val newBlock = Block(Seq(
  stmt1,
  stmt2,
  Assert(condition)(blame),
  stmt3
))
```

### Common Statement Types for Insertion

```scala
// Assertion
Assert(expression)(blame)

// Inhale (assume and get permission)
Inhale(expression)

// Exhale (lose permission)
Exhale(expression)(blame)

// Assume
Assume(expression)

// Assignment
Assign(target, value)(blame)

// Fold/Unfold
Fold(predicateApplication)(blame)
Unfold(predicateApplication)(blame)

// Goto
Goto(labelRef)

// Scope with local variables
Scope(locals, body)
```

---

## Complete Examples

### Example 1: EncodeIntrinsicLock.scala - Lock Statement

This example shows how Assert statements are inserted alongside other statements:

```scala
// From EncodeIntrinsicLock.scala, lines 214-228
case lock @ Lock(obj) =>
  if (needsInvariant(obj))
    Block(Seq(
      // Create assertion with custom blame
      Assert(getCommitted(obj)(LockLockObjectNull(lock)))(
        NotCommittedAssertFailed(lock)
      ),
      // Then insert permission operations
      Inhale(Perm(PredicateLocation(getInvariant(obj)), WritePerm())),
      Unfold(ScaledPredicateApply(getInvariant(obj), WritePerm()))(
        PanicBlame(
          "Unfolding a predicate immediately after inhaling it should never fail."
        )
      ),
      Inhale(Perm(PredicateLocation(getHeld(obj)), WritePerm())),
    ))
  else
    Block(Seq(
      Assert(getCommitted(obj)(LockLockObjectNull(lock)))(
        NotCommittedAssertFailed(lock)
      ),
      Inhale(Perm(PredicateLocation(getHeld(obj)), WritePerm())),
    ))
```

**Key patterns:**
- Assert is the first statement (to verify preconditions)
- Custom `Blame` object maps domain-specific errors
- Origin is implicitly available in dispatch method
- Multiple statements are combined in `Block(Seq(...))`

---

### Example 2: InlineApplicables.scala - Fold Assertion

This example shows Assert with predicates and permissions:

```scala
// From InlineApplicables.scala, lines 210-216
override def dispatch(stat: Statement[Pre]): Statement[Post] =
  stat match {
    case f @ Fold(target @ ScaledPredicateApply(inv, perm))
        if inv.ref.decl.inline =>
      // Assert that we have permission for the predicate before folding
      Assert(permExpression(PredicateLocation(inv)(f.o), perm, target.o))(
        InlineFoldAssertFailed(f)
      )(stat.o)  // Explicit origin from original statement
      
    case u @ Unfold(target @ ScaledPredicateApply(inv, perm))
        if inv.ref.decl.inline =>
      Assert(permExpression(PredicateLocation(inv)(u.o), perm, target.o))(
        InlineUnfoldAssertFailed(u)
      )(stat.o)

    case other => other.rewriteDefault()
  }
```

**Key patterns:**
- Assert replaces the entire statement
- Custom blame uses pattern matching on error types
- Explicit origin can be provided as third parameter
- Uses `f.o` to create assertion about fold's location

---

### Example 3: RefuteToInvertedAssert.scala - Assert in Inverted Branch

This example shows Assert inserted into a newly created Block:

```scala
// From RefuteToInvertedAssert.scala, lines 55-70
override def dispatch(stat: Statement[Pre]): Statement[Post] =
  stat match {
    case refute @ Refute(assn) =>
      implicit val o: Origin = stat.o
      val err = ExpectedError(
        "assertFailed:.*",
        stat.o,
        AssertPassedRefuteFailed(refute),
      )
      expectedErrors.top += err
      
      // Create a branch with assertion
      IndetBranch(Seq(
        Block(Seq(
          // Assert the condition is true (for expected error)
          Assert(dispatch(assn))(FilterExpectedErrorBlame(
            PanicBlame("wrong assert error kind"),
            err,
          )),
          Inhale(ff),  // Force unreachable with false
        )),
        Block(Nil),  // Alternative branch (do nothing)
      ))

    case other => rewriteDefault(other)
  }
```

**Key patterns:**
- Assert is created and immediately placed in a Block
- Origin is made implicit for convenience
- Complex blame object created inline
- Multiple statements combined in sequence

---

### Example 4: EncodeTryThrowSignals.scala - Assert with Null Check

This example shows Assert with comparison expressions:

```scala
// From EncodeTryThrowSignals.scala, lines 171-174
case t @ Throw(obj) =>
  Block(Seq(
    assignLocal(getExc, dispatch(obj)),
    Assert(getExc !== Null())(ThrowNullAssertFailed(t)),
    Goto(exceptionalHandlerEntry.top.ref),
  ))
```

**Key patterns:**
- Uses `!==` operator to create inequality expression
- Simple assertion with straightforward blame
- Asserts before performing operations
- Combines with assignment and control flow

---

### Example 5: EncodeTryThrowSignals.scala - Assert with Panic Blame

```scala
// From EncodeTryThrowSignals.scala, line 299
Assert(exc.get === Null())(AssertFailedSignalsNotClosed(method))
```

---

## Helper Functions

### AstBuildHelpers Functions

Located in `src/col/vct/col/util/AstBuildHelpers.scala`:

```scala
// Create local assignment
def assignLocal[G](local: Local[G], value: Expr[G])(
    implicit o: Origin
): Assign[G]

// Create field permission expression
def fieldPerm[G](
    obj: Expr[G],
    field: Ref[G, InstanceField[G]],
    amount: Expr[G],
)(implicit o: Origin): Perm[G]

// Create array permission expression
def arrayPerm[G](
    arr: Expr[G],
    index: Expr[G],
    amount: Expr[G],
    arrayLocationError: Blame[ArrayLocationError],
)(implicit o: Origin): Perm[G]

// Fold multiple boolean expressions with AND
def foldAnd[G](exprs: Iterable[Expr[G]])(implicit o: Origin): Expr[G]
```

### Built-in Expression Operators (from ExprBuildHelpers)

These are implicit methods that create expressions:

```scala
implicit class ExprBuildHelpers[G](left: Expr[G]) {
  def +(right: Expr[G])(implicit origin: Origin): Plus[G]
  def -(right: Expr[G])(implicit origin: Origin): Minus[G]
  def *(right: Expr[G])(implicit origin: Origin): Mult[G]
  def ===(right: Expr[G])(implicit origin: Origin): Eq[G]
  def !==(right: Expr[G])(implicit origin: Origin): Neq[G]
  def <(right: Expr[G])(implicit origin: Origin): Less[G]
  def >(right: Expr[G])(implicit origin: Origin): Greater[G]
  def <=(right: Expr[G])(implicit origin: Origin): LessEq[G]
  def >=(right: Expr[G])(implicit origin: Origin): GreaterEq[G]
  def unary_!(implicit origin: Origin): Not[G]
  def &&(right: Expr[G])(implicit origin: Origin): And[G]
  def ||(right: Expr[G])(implicit origin: Origin): Or[G]
}
```

---

## Required Imports

```scala
// AST classes
import vct.col.ast._
import vct.col.ast.RewriteHelpers._

// Error and blame types
import vct.col.origin._

// Reference types
import vct.col.ref.Ref

// Helper functions
import vct.col.util.AstBuildHelpers._

// Rewriter infrastructure
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
```

---

## Summary: Creating Assert Statements

### Step-by-step pattern:

1. **Get or create an Origin**
   ```scala
   implicit val o: Origin = existingNode.o
   // or
   implicit val o: Origin = SomeOrigin()
   ```

2. **Create a Blame object** (either built-in or custom)
   ```scala
   val blame = PanicBlame("message")
   // or
   case class MyAssertFailed(node: MyNode[_]) 
       extends Blame[AssertFailed] { ... }
   ```

3. **Create the Assert statement**
   ```scala
   Assert(expression)(blame)
   // The origin is implicitly available
   ```

4. **Insert into Block if needed**
   ```scala
   Block(Seq(
     Assert(expr1)(blame1),
     Assert(expr2)(blame2),
     // ... other statements
   ))
   ```

### Important Constraints:

- **Origin is implicit**: In rewriter dispatch methods, Origin is always implicitly available from `implicit val o: Origin`
- **Blame is required**: Assert always requires a Blame object for error reporting
- **Parametrization**: Assert[G] must match the generation parameter (Pre or Post)
- **Statements are immutable**: Create new statements rather than modifying existing ones

---

## References

- **Node Definitions**: `src/col/vct/col/ast/Node.scala` (lines 440-620)
- **Blame Definitions**: `src/col/vct/col/origin/Blame.scala`
- **Example Rewriter 1**: `src/rewrite/vct/rewrite/EncodeIntrinsicLock.scala`
- **Example Rewriter 2**: `src/rewrite/vct/rewrite/InlineApplicables.scala`
- **Example Rewriter 3**: `src/rewrite/vct/rewrite/RefuteToInvertedAssert.scala`
- **Example Rewriter 4**: `src/rewrite/vct/rewrite/exc/EncodeTryThrowSignals.scala`
- **Helper Functions**: `src/col/vct/col/util/AstBuildHelpers.scala`
