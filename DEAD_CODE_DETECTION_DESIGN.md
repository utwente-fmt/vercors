# Dead Code Detection in VerCors — Design Document

**Branch:** `fragma-c-tests`  
**Pass:** `DetectDeadCode` (`src/rewrite/vct/rewrite/DetectDeadCode.scala`)  
**Pipeline position:** After `CollectLocalDeclarations` and `PropagateContextEverywhere`, before `RefuteToInvertedAssert` and `CheckPostconditionSatisfiability`.

---

## Background and Goal

Static verification tools like VerCors prove that a program satisfies its specification. A natural by-product of that work is that certain code paths can be identified as statically unreachable under a method's preconditions: if the precondition already implies a contradiction, or if an assumption inside the method narrows the state enough that a subsequent branch condition can never hold, then that branch is dead. Failing to report dead code is a usability problem — a programmer may have placed a dead branch by accident, and the verifier would silently accept it.

The `DetectDeadCode` pass is a rewrite pass that instruments the AST before it reaches the backend (Silicon), inserting probes at positions that may be dead. The backend then determines, as part of its ordinary verification work, whether those probes are reachable.

---

## How the Pass Fits into the Pipeline

VerCors transforms the source program through a sequence of rewrite passes, each taking a COL (Common Object Language) AST and producing a new one. `DetectDeadCode` runs after the program has been fully resolved and simplified but before it is handed to Viper. After `DetectDeadCode` runs, `RefuteToInvertedAssert` translates the `Refute` nodes it inserted into actual Silver assertions, and those assertions are then verified by Silicon.

`PrettifyBlocks` runs after every pass, flattening nested `Block(Block(...))` nodes and removing empty `Scope(Nil, Block(Nil))` wrappers, so the AST seen by the next pass is always in a canonical form.

---

## Design Decision 1: Using `Refute(false)` as the Reachability Probe

### How it works

The pass inserts a `Refute(false)` statement at the entry of every code region that may be dead. `Refute(P)` is a VerCors statement that asserts that `P` cannot be proven in the current state — it is a "prove this is not provable" check. When `P` is `false`, `Refute(false)` asserts that `false` cannot be proven, which is equivalent to asserting that the current program state is satisfiable (i.e., the path is reachable). If the state is unsatisfiable (the path is dead), then `false` is trivially provable from a contradiction, and the `Refute(false)` fails.

This means:
- `Refute(false)` **passes** → the state is satisfiable → the code is reachable → no dead code warning
- `Refute(false)` **fails** → the state is unsatisfiable → the code is unreachable → dead code warning

`RefuteToInvertedAssert`, which runs after this pass, translates each `Refute(false)` into the appropriate Silver construct that Silicon can check.

### Why this approach

The key advantage is that we do not need to implement any reachability analysis ourselves. Reachability under a general first-order specification is undecidable in general and requires SMT reasoning for any non-trivial case. By inserting `Refute(false)` probes and delegating to Silicon, we get the full power of Silicon's Z3-backed reasoning for free. The pass itself does nothing more than AST transformation — it adds nodes; Silicon decides whether they succeed or fail.

The alternative would be to implement a lightweight abstract interpretation or syntactic reachability analysis inside VerCors. This would be far less precise (many false negatives — dead code that the analysis misses) and would duplicate reasoning already done by the backend.

### Code

The two helpers that create and insert probes:

```scala
private def makeCheck(
    node: Node[Pre],
    branchKind: String,
    extraSuppressor: () => Boolean = () => false,
): (DeadCodeBlame, Statement[Post]) = {
  implicit val o: Origin = node.o.where(prefix = "deadCode")
  val ancestorFiredFn: () => Boolean = {
    val base = currentBlame.topOption.map(b => () => b.didFire).getOrElse(() => false)
    () => base() || extraSuppressor()
  }
  val blame = new DeadCodeBlame(node, branchKind, methodBlame.top, ancestorFiredFn)
  (blame, Refute(ff)(blame))   // ff = BooleanValue(false)
}

private def instrumentBody(
    node: Node[Pre],
    label: String,
    body: Statement[Pre],
    ...
)(implicit o: Origin): Statement[Post] = {
  val (blame, check) = makeCheck(origin, label, extraSuppressor)
  currentBlame.having(blame) { Block(Seq(check, dispatch(body))) }
}
```

`instrumentBody` prepends the probe to the body. This is important: the probe runs in the state that precedes the body, so it checks whether that entry state is satisfiable, which is exactly the question "is this branch reachable?". Placing it inside the body would risk missing state transformations that happen before the first statement.

The same probe-then-body structure is used for branch bodies, loop bodies, switch cases, and parallel block bodies:

```scala
// Branch with else
case branch @ Branch(Seq((cond, thenBody), (BooleanValue(true), elseBody))) =>
  Branch(Seq(
    (dispatch(cond),
      instrumentBody(branch, s"then-branch (condition: `${condText(cond)}`)", thenBody, originNode = thenBody)),
    (dispatch(BooleanValue(true)),
      instrumentBody(branch, s"else-branch (negation of condition: `${condText(cond)}`)", elseBody, originNode = elseBody)),
  ))

// Loop body
case loop @ Loop(init, cond, update, contract, body) =>
  Loop(
    dispatch(init), dispatch(cond), dispatch(update),
    dispatch(contract),
    instrumentBody(loop, s"loop body (condition: `${condText(cond)}`...)", body),
  )
```

For state-narrowing statements like `assume` and `inhale`, the probe comes *after* the statement rather than before, because the relevant question is whether the code that follows is still reachable after the state has been narrowed:

```scala
private def appendCheck(node: Statement[Pre], label: String): Statement[Post] = {
  implicit val o: Origin = node.o
  val (blame, check) = makeCheck(node, label)
  Block(Seq(node.rewriteDefault(), currentBlame.having(blame) { check }))
}

case a @ Assume(assn) => appendCheck(a, s"code after assume `${condText(assn)}`")
case i @ Inhale(res)  => appendCheck(i, s"code after inhale `${condText(res)}`")
```

---

## Design Decision 2: Three-Way Semantics for False-Valued Statements

### The three cases

VerCors source programs can contain three different kinds of "false" statement: `assume false`, `inhale false`, and `assert false`. The pass treats them entirely differently, because they have different intended meanings.

| Source statement | COL node after parsing | Treatment in DetectDeadCode | Effect in Silicon |
|---|---|---|---|
| `assume false` | `Assume(BooleanValue(false))` | Rewritten to `Inhale(ff)`; block cutoff applied; no warning | State becomes ⊥ |
| `inhale false` | `Inhale(BooleanValue(false))` | Immediate `DeadBranch` blame emitted at rewrite time; statement kept as-is | State becomes ⊥ |
| `assert false` | `Assert(BooleanValue(false))` | No special treatment; falls through to `rewriteDefault()` | Silicon reports `assertFailed` |

### `assume false` — the intentional dead code marker

`assume false` is treated as a deliberate programmer signal that the current path is intentionally dead. When a programmer writes `assume false`, they are saying: "I know execution cannot reach this point; please do not instrument the continuation." The pass respects this by:

1. Rewriting the `Assume(false)` to `Inhale(ff)` — which makes the state ⊥ in Silicon and vacuously verifies everything that follows, without inserting an unwanted probe after the narrowed state.
2. Applying a block cutoff (see Decision 3 below) that suppresses instrumentation of all statements after it in the same block.

This means that if a programmer deliberately marks a path with `assume false`, they get zero dead code warnings for that path, even if the code after it would otherwise trigger probes.

```scala
case a @ Assume(BooleanValue(false)) =>
  implicit val o: Origin = a.o
  Inhale(ff)   // state → ⊥; no probe appended; block cutoff handled by Block case
```

### `inhale false` — the explicit specification statement that always warns

`inhale false` is a specification-level construct with a different intent. Unlike `assume false`, it is not a dead code marker — it is an explicit change to the heap/state that happens to make the state contradictory. Reaching an `inhale false` is always a sign of a specification error. The pass emits a `DeadBranch` blame immediately at rewrite time (before Silicon even runs), because the answer is statically certain: if `inhale false` is reachable, something is wrong.

Crucially, there is no block cutoff for `inhale false`. Code after it is still instrumented normally, and since Silicon's state is ⊥ after `inhale false`, any `Refute(false)` probes placed after it will fire, generating additional dead code warnings for each subsequent region. This is intentional: the user needs to know not just that `inhale false` was reached, but also exactly which downstream code became dead as a result.

```scala
case i @ Inhale(BooleanValue(false)) =>
  methodBlame.top.blame(DeadBranch(i, "code after inhale false"))
  i.rewriteDefault()   // keep the Inhale in the AST; no block cutoff
```

### `assert false` — handled entirely by Silicon

`assert false` has no special treatment in this pass. It falls through to `other.rewriteDefault()`, which means the `Assert(BooleanValue(false))` is passed through to Silicon unchanged. Silicon will report an `assertFailed` error for it. The dead code detection pass does not interfere, because `assert false` is not a dead code marker — it is a verification failure that the programmer did not suppress.

The distinction from `assume false` is important: `assert false` says "this should not be provable, but it is" (a bug), whereas `assume false` says "this path is intentionally dead" (a deliberate annotation). Conflating them would cause the pass to either suppress real bugs or warn about intentionally dead paths.

---

## Design Decision 3: Block Cutoff After `assume false`

### How it works

The `Block` case of `dispatch` scans the statement list for the first occurrence of `Assume(BooleanValue(false))`. If found, it splits the list at that index:

- Statements **before and including** the `assume false` are dispatched normally (with instrumentation).
- Statements **after** the `assume false` are dispatched with the `afterAssertFalse` stack active.

When `afterAssertFalse` is non-empty, the very first check in `dispatch` returns the statement unchanged without any instrumentation:

```scala
override def dispatch(stat: Statement[Pre]): Statement[Post] =
  if (methodBlame.topOption.isEmpty || afterAssertFalse.nonEmpty) stat.rewriteDefault()
  else stat match { ... }
```

This means that any statement after `assume false` — whether it is a branch, a loop, another assume, or anything else — is passed through to the backend as-is, with no `Refute(false)` probe inserted.

```scala
case block @ Block(stmts) =>
  val cutoffIdx = stmts.indexWhere {
    case Assume(BooleanValue(false)) => true
    case _ => false
  }
  if (cutoffIdx < 0) {
    Block(stmts.map(dispatch))
  } else {
    val (before, rest) = stmts.splitAt(cutoffIdx + 1)
    Block(before.map(dispatch) ++ afterAssertFalse.having(()) { rest.map(dispatch) })
  }
```

### Why this design

Without the block cutoff, every statement after `assume false` would receive a `Refute(false)` probe, and every single one of those probes would fire — because the state after `assume false` is ⊥ and everything follows from it. This would produce an avalanche of dead code warnings for a single intentional annotation.

The cutoff is placed at the `Block` level rather than implemented as a recursive flag because it only needs to suppress instrumentation within the current block. Nested blocks (e.g., the body of a branch or loop that appears after the `assume false`) are also suppressed because `afterAssertFalse.nonEmpty` is checked at the top of every `dispatch` call, and the `afterAssertFalse` scope remains active for the entire `rest.map(dispatch)` call, which recursively dispatches all nested content.

The cutoff is intentionally only triggered by `Assume(BooleanValue(false))` and not by `Inhale(BooleanValue(false))` or `Assert(BooleanValue(false))`, because only `assume false` is the intentional dead code marker. For `inhale false`, we want downstream warnings (see Decision 2). For `assert false`, Silicon handles it, and the continuation should still be instrumented because Silicon continues verification from the pre-assertion state.

---

## Design Decision 4: Cascade Suppression via `DeadCodeBlame`

### The problem

Consider a method with a precondition that makes the method body unreachable:

```pvl
requires false;
void f(int x) {
    if (x > 0) {
        while (x < 10) {
            x = x + 1;
        }
    }
}
```

After instrumentation, there would be a `Refute(false)` at the start of the `if`-branch, a `Refute(false)` at the start of the loop body, and so on. Since `requires false` makes the entire method body unreachable, every single probe would fire in Silicon. Without suppression, the user would receive one dead code warning per probe — many warnings for a single root cause.

### How `DeadCodeBlame` works

Every `Refute(false)` probe is paired with a `DeadCodeBlame` object. `DeadCodeBlame` holds:

- A reference to the AST node that the probe is protecting (for error reporting)
- A reference to the method-level blame (where `DeadBranch` errors are ultimately reported)
- An `ancestorFired` function that checks whether any enclosing probe has already fired

```scala
class DeadCodeBlame(
    branchNode: Node[_],
    branchKind: String,
    methodBlame: Blame[ContractedFailure],
    ancestorFired: () => Boolean,
) extends Blame[RefuteFailed] {
  private var firedOrSuppressed: Boolean = false

  def didFire: Boolean = firedOrSuppressed

  override def blame(error: RefuteFailed): Unit = {
    firedOrSuppressed = true
    if (!ancestorFired())
      methodBlame.blame(DeadBranch(branchNode, branchKind))
  }
}
```

When a probe fires, `firedOrSuppressed` is set to true regardless of whether the error is reported. This is important: even if the probe is suppressed (because an ancestor already fired), it still marks itself as fired, so that its own descendants can suppress themselves in turn.

The `ancestorFired` function is built in `makeCheck` by capturing the current top of the `currentBlame` stack:

```scala
private def makeCheck(...): (DeadCodeBlame, Statement[Post]) = {
  val ancestorFiredFn: () => Boolean = {
    val base = currentBlame.topOption.map(b => () => b.didFire).getOrElse(() => false)
    () => base() || extraSuppressor()
  }
  val blame = new DeadCodeBlame(node, branchKind, methodBlame.top, ancestorFiredFn)
  (blame, Refute(ff)(blame))
}
```

The `currentBlame` stack is pushed in `instrumentBody`, so that while dispatching the contents of a branch or loop body, `currentBlame.top` refers to that body's own probe blame:

```scala
currentBlame.having(blame) { Block(Seq(check, dispatch(body))) }
```

This creates a tree of blame objects mirroring the nesting structure of the program. When the outermost probe fires, every inner probe will see `ancestorFired() == true` and suppress its own `DeadBranch` report.

### Why a lazy function rather than a direct reference

The `ancestorFired` check is a function `() => Boolean` rather than a direct reference to a `Boolean` field. This is because the probes are created during the rewrite pass (at AST transformation time) but they fire later, during Silicon verification. At creation time, no probe has fired yet — `didFire` is always false. The function captures the blame object by reference and evaluates `didFire` at the moment Silicon calls the blame, which is the correct time to check whether the ancestor fired.

---

## Design Decision 5: Scope Wrapping of Loops — An Implementation Finding

### The finding

During development, intermediate COL dumps were inspected at `tmp/cols/verify-105-after-detectDeadCode.col`. The dump showed that no `Refute(false)` probe was being inserted at the start of a loop body, even though the loop case in `dispatch` clearly called `instrumentBody`. Investigation revealed that the `Loop` node was never being matched.

The root cause: all three parsers (PVL, Java, C) wrap every loop statement in a `Scope` node. PVL produces:

```scala
Scope(Nil, PVLLoop(Block(Nil), cond, Block(Nil), contract, body))
// desugared by LangSpecificToCol to:
Scope(Nil, Loop(init, cond, update, contract, body))
```

Java and C produce the same structure. `Scope(Nil, Loop(...))` is not the same AST node as a bare `Loop(...)`, so the `case loop @ Loop(...)` pattern in `dispatch` was receiving the `Scope`, not the `Loop`, and falling through to `other.rewriteDefault()`.

The fix was straightforward: the `Scope` case falls through to `other.rewriteDefault()`, which calls `dispatch` recursively on the `Scope`'s body, eventually reaching the `Loop`. This meant the `Loop` case was already correct — the issue was only visible in contexts where `suppressPostLoopCheck` (a mechanism since removed) needed to intercept the outermost statement. The loop body instrumentation itself was working all along; only the suppression mechanism was broken.

### Why this matters for the design

This finding established an important principle for pattern matching in this pass: patterns on statement types must account for the `Scope` wrapper. A bare `Loop` never appears as a top-level statement in practice. Any future pattern matching that needs to detect "the last statement is a loop" must match `Scope(_, _: Loop[Pre])`, not `_: Loop[Pre]`.

---

## Design Decision 6: Post-Loop Check — Considered and Rejected

### What was proposed

An earlier version of the pass inserted a `Refute(false)` probe after loop bodies, in addition to the probe inside the body. The intent was to catch cases where the post-loop state is unreachable — for example, if the loop invariant is unsatisfiable, Silicon adds `assume false` after the invariant establishment failure, making the loop body and all code after the loop appear unreachable.

The implementation was substantial. It required:

1. A `suppressPostLoopCheck` stack to suppress the post-loop probe when the loop is the last statement (there is nothing to protect after it).
2. An `invNotEstablishedFired` flag to suppress cascade artifacts when the loop invariant failed to be established — because if the invariant fails, Silicon makes both the body and the post-loop state appear unreachable as a cascade, not because either is genuinely dead.
3. A `LoopInvariantFailure` blame interceptor that set the flag when `LoopInvariantNotEstablished` fired.

The `suppressPostLoopCheck` mechanism itself contained a bug: it matched `case _: Loop[Pre] if isLast` but, as described in Decision 5, bare `Loop` nodes never appear at the statement level — they are always wrapped in `Scope(_, Loop(...))`. The suppression never triggered, causing the post-loop probe to be inserted even when the loop was the last statement.

The code that was removed:

```scala
// Formerly in the Loop case:
var invNotEstablishedFired = false
val (invInfo, dispatchedContract) = contract match {
  case li: LoopInvariant[Pre] =>
    val interceptBlame = new Blame[LoopInvariantFailure] {
      override def blame(error: LoopInvariantFailure): Unit = {
        if (error.isInstanceOf[LoopInvariantNotEstablished])
          invNotEstablishedFired = true
        li.blame.blame(error)
      }
    }
    (s", invariant: `${condText(li.invariant)}`",
     LoopInvariant(dispatch(li.invariant), li.decreases.map(dispatch))(interceptBlame))
  case other => ("", dispatch(other))
}
val postLoopSuppressor: () => Boolean = () => invNotEstablishedFired
val (postBlame, postCheck) = makeCheck(
  loop,
  s"code after loop (condition: `${condText(cond)}`$invInfo)",
  postLoopSuppressor,
)
// ...
if (suppressPostLoopCheck.nonEmpty)
  dispatchedLoop
else
  Block(Seq(dispatchedLoop, currentBlame.having(postBlame) { postCheck }))
```

### Why it was removed

**False positives.** The post-loop state is almost always satisfiable. If a loop has a meaningful invariant (which is the common case in verified programs), the state after the loop is the invariant conjoined with the negation of the loop condition — a perfectly satisfiable state. The `Refute(false)` probe would always pass, generating a verification obligation that Silicon has to discharge for every loop in every method, with no benefit.

**Overhead.** Every loop in every method would acquire an extra Silver assertion that Silicon must check. For programs with many loops, this compounds the verification time meaninglessly.

**Complexity.** The cascade suppression for `invNotEstablishedFired` was a non-trivial addition. The `suppressPostLoopCheck` mechanism added another `ScopedStack` and a pattern match that was silently broken. The invariant-not-established case that the feature was meant to handle is already reported to the user by the loop invariant check itself; the post-loop dead code finding adds nothing the user does not already know.

The current `Loop` case after the removal:

```scala
case loop @ Loop(init, cond, update, contract, body) =>
  implicit val o: Origin = loop.o
  val (invInfo, dispatchedContract) = contract match {
    case li: LoopInvariant[Pre] =>
      (s", invariant: `${condText(li.invariant)}`", dispatch(li))
    case other => ("", dispatch(other))
  }
  Loop(
    dispatch(init), dispatch(cond), dispatch(update),
    dispatchedContract,
    instrumentBody(loop, s"loop body (condition: `${condText(cond)}`$invInfo)", body),
  )
```

The loop case now only instruments the body entry — which is the check that actually matters. The loop body can be dead if the loop condition is never satisfiable, and that is exactly what the body probe checks.

---

## Summary

`DetectDeadCode` is a targeted, backend-delegating pass. Its design is driven by three core principles:

1. **Delegate reasoning to the backend.** Reachability under a general specification requires SMT reasoning. The pass inserts probes and lets Silicon decide; it does not attempt any independent analysis.

2. **Respect programmer intent.** The three-way treatment of `assume false` / `inhale false` / `assert false` exists because these statements have distinct meanings. `assume false` is a deliberate suppression; `inhale false` is a specification-level error that always warrants a warning; `assert false` is a verification failure that Silicon handles natively.

3. **Report root causes, not cascades.** The `DeadCodeBlame` hierarchy ensures that when a region is dead, only the outermost dead code finding is reported. Post-loop checking was removed precisely because it violated this principle in a different way — it added overhead for a check that almost never identifies a root cause not already covered by the invariant failure that preceded it.
