package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class SmokeTestSpec extends VercorsSpec {
  // Parallel block dead code
  // Fail: tid is always >= 0 inside par body — branch tid < 0 is dead
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-tests/cat2-10-parallel/TC-PB-1-parallel-dead-branch.pvl"
  // Fail: range [0..size) with size <= 0 is empty — par body is unreachable
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-tests/cat2-10-parallel/TC-PB-2-par-body-dead.pvl"
  // Pass: range is non-empty, body is reachable
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-10-parallel/TC-PB-3-parallel-body-live.pvl"

  // Pass: branch inside par body is live (tid < n is satisfiable for tid in [0..n))
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-10-parallel/TC-PB-4-par-live-branch-inside.pvl"
  // Fail: context_everywhere adds invariant to each iteration — branch contradicting it is dead
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-tests/cat2-10-parallel/TC-PB-5-par-context-everywhere-dead.pvl"

  // Atomic block dead code
  // Fail: invariant x > 0 is inhaled — branch x < 0 inside atomic is dead
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-tests/cat2-9-atomic/TC-AT-3-atomic-dead-branch.pvl"
  // Pass: body is consistent with invariant — no dead code
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-9-atomic/TC-AT-4-atomic-body-live.pvl"
  // Fail: complex invariant (x > 0 && y > 0) — branch x < 0 is dead
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-tests/cat2-9-atomic/TC-AT-5-atomic-complex-invariant-dead.pvl"

  // Atomic entry-level dead code (atomic treated as state-narrowing, like lock obj).
  // Not precondition-driven: root cause is permission duplication from re-entering
  // an already-held invariant, so these live in their own category.
  // Fail: nested atomic on the same invariant duplicates its permission — inner atomic is dead
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-tests/cat2-9-atomic/TC-AT-1-atomic-reentrant-dead.pvl"
  // Pass: single, non-nested atomic with a permission-bearing invariant — no false positive
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-9-atomic/TC-AT-2-atomic-perm-invariant-live.pvl"

  // Postcondition satisfiability (CheckPostconditionSatisfiability)
  // The checker inhales the postcondition and asserts false, expecting that assertion to fail.

  // Pass (PVL): satisfiable postconditions — checker must not fire
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-1-ensures-satisfiable.pvl"
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-6-ensures-stricter-satisfiable.pvl"
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-7-ensures-satisfiable-void.pvl"
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-8-ensures-result-range-sat.pvl"
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-10-requires-narrows-ensures-sat.pvl"

  // Fail (PVL): postcondition contradicts itself — postUnsatisfiable fires (plus postFailed from normal verification)
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-2-ensures-unsat-contradiction.pvl"
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-4-ensures-result-contradiction.pvl"
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-5-ensures-perm-false.pvl"
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-9-ensures-n-gt-n.pvl"

  // Pass (Java): satisfiable postconditions — checker must not fire
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-12-java-ensures-satisfiable.java"
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-14-java-requires-conflicts-ensures.java"

  // Fail (Java): postcondition contradicts itself — checker must fire with postUnsatisfiable
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-13-java-ensures-unsat.java"

  // Pass (C): satisfiable postconditions — checker must not fire
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-15-c-ensures-satisfiable.c"

  // Fail (C): postcondition contradicts itself — checker must fire with postUnsatisfiable
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-16-c-ensures-unsat.c"

  // Postcondition calls a pure function.
  // Pass: foo's precondition satisfies callee's requirement; postcondition is satisfiable.
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-17-ensures-function-call-sat.pvl"
  // Fail: function body is always false (no preconditions) — postUnsatisfiable fires (plus postFailed).
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-18-ensures-function-call-unsat.pvl"
  // Pass: Perm in ensures — axiom fires, postcondition is satisfiable.
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-19-ensures-perm-function-call-sat.pvl"
  // Fail: Perm in ensures — axiom fires, body always false — postUnsatisfiable fires (plus postFailed).
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-20-ensures-perm-function-call-unsat.pvl"

  // Edge cases for postcondition calls
  // \old in postcondition: sat checker must not fire, handled via eliminateOldInTriggers.
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-21-old-in-ensures-pass.pvl"
  // \old in forall range: the exact pattern from ArrayList.java that previously crashed.
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-22-old-in-ensures-forall-range-pass.pvl"
  // \old on a heap location (array element) inside a forall in ensures.
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-30-old-heap-in-ensures-forall-pass.pvl"
  // Nested calls: axioms fire unconditionally, postcondition is satisfiable.
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-22-nested-function-calls-sat.pvl"
  // Nested calls where inner function negates — isPos(n) && isPos(negate(n)) is always false.
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-23-nested-function-calls-unsat.pvl"
  // Instance function call: Perm is in ensures, axiom fires, postcondition is satisfiable.
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-24-instance-function-call-sat.pvl"

  // Heap field in postcondition
  // Pass: body writes field to 0; postcondition Perm + n.val == 0 is satisfiable.
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-27-heap-field-write-pass.pvl"
  // Fail: postcondition claims n.val > 0 AND n.val < 0 — field value contradiction.
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-28-heap-field-contradiction-fail.pvl"
  // Pass: heap precondition (n.val > 0) filtered by isNonHeap — postcondition still
  // independently satisfiable, demonstrating the SilverDeref fix.
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-29-heap-precond-filtered-pass.pvl"

  // Non-heap preconditions from foo's own requires are added as assumptions.
  // Fail: foo requires n > 0, but ensures isNeg(n) (= n < 0) — contradiction only visible with fix.
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-25-requires-nonheap-conflicts-ensures-fn-unsat.pvl"
  // Pass: foo requires n > 0, ensures isPos(n) (= n > 0) — compatible, postSat must not fire.
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-26-requires-nonheap-compatible-ensures-fn-sat.pvl"

  // extractNonHeap: precondition is a Select (ternary) mixing a permission with a pure
  // fact — the pure parts are extracted and checked against the postcondition.
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-7-postcondition/TC-PST-31-requires-select-extracts-nonheap-unsat.pvl"

  // Loop invariant + post-loop dead code (DetectDeadCode)
  // Dead if-branch after loop: invariant+exit makes branch condition impossible
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-1-post-loop-dead.pvl"
  // Negative: post-loop branch is live — no error
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-4-negative-consistent.pvl"
  // Post-loop state false: invariant implies !condition, code after loop is unreachable
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-6-post-loop-sequential-dead.pvl"
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-7-post-loop-invariant-stronger-than-condition.pvl"
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-8-post-loop-context-everywhere-dead.pvl"
  // Negative: loop genuinely terminates, post-loop state satisfiable
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-10-post-loop-negative-terminates.pvl"

  // Loop invariant satisfiability (CheckInvariantSatisfiability)
  // Pass (PVL): satisfiable invariants — checker must not fire
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-11-invariant-satisfiable-pass.pvl"
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-18-invariant-with-perm-satisfiable.pvl"

  // Fail (PVL): unsatisfiable invariants — checker must fire with invariantUnsatisfiable
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-12-invariant-false-unsat.pvl"
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-13-invariant-contradiction-unsat.pvl"
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-19-nested-loop-both-checked.pvl"

  // Pass (Java): satisfiable invariant — checker must not fire
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-14-java-invariant-satisfiable.java"

  // Fail (Java): unsatisfiable invariant — checker must fire with invariantUnsatisfiable
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-15-java-invariant-contradiction-unsat.java"

  // Pass (C): satisfiable invariant — checker must not fire
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-16-c-invariant-satisfiable.c"

  // Fail (C): unsatisfiable invariant — checker must fire with invariantUnsatisfiable
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-17-c-invariant-contradiction-unsat.c"

  // Well-definedness conditions fix for loop invariants
  // Pass: invariant calls a pure function with precondition, invariant is satisfiable.
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-20-invariant-function-call-sat.pvl"
  // Fail: invariant calls a pure function always returning false for valid inputs.
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-21-invariant-function-call-unsat.pvl"
  // Nested calls in invariant: depth-first WD ordering, satisfiable.
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-22-invariant-nested-function-calls-sat.pvl"

  // Permission WD conditions in invariants
  // Pass: invariant calls function with write permission precondition, satisfiable.
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-23-invariant-perm-function-call-sat.pvl"
  // Fail: invariant calls function with write permission, body always false — WD fix detects it.
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-24-invariant-perm-function-call-unsat.pvl"
  // Bug demo: invariant explicitly includes Perm + WD fix also adds Perm = 200% overcounting.
  // Expected Pass but current WD fix incorrectly fires invariantUnsatisfiable.
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-4-loop-invariant/TC-LI-25-invariant-perm-overcount-bug.pvl"

  // Cascade suppression: all live loop with invariant established and maintained
  vercors should verify using silicon example "concepts/smoke-test-tests/cat3-cascade/TC-CA-13-negative-loop-all-live.pvl"

  // Block (parallel) invariant satisfiability (CheckInvariantSatisfiability, ParInvariant case)
  // Pass: satisfiable invariants — checker must not fire
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-8-block-invariant/TC-BI-1-invariant-satisfiable-pass.pvl"
  vercors should verify using silicon example "concepts/smoke-test-tests/cat2-8-block-invariant/TC-BI-3-invariant-with-perm-satisfiable.pvl"

  // Fail: unsatisfiable invariants — checker must fire with parInvariantUnsatisfiable
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-8-block-invariant/TC-BI-2-invariant-contradiction-unsat.pvl"
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-8-block-invariant/TC-BI-4-invariant-perm-duplicate-unsat.pvl"
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat2-8-block-invariant/TC-BI-5-nested-block-invariant-unsat.pvl"

  // assert false as intentional dead code marker — block cutoff, assert itself fails
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat5-assert-false/TC-AF-1-pvl-assert-false-mid-block.pvl"
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat5-assert-false/TC-AF-2-pvl-assert-false-in-loop-body.pvl"
  vercors should AnyFail using silicon example "concepts/smoke-test-tests/cat5-assert-false/TC-AF-5-java-assert-false-in-branch.java"
  vercors should fail withCode "assertFailed:false" using silicon example "concepts/smoke-test-tests/cat5-assert-false/TC-AF-7-c-assert-false-mid-block.c"

  // assume false and inhale false produce deadBranch warnings
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-tests/cat5-assert-false/TC-AF-3-pvl-assume-false-gives-warning.pvl"
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-tests/cat5-assert-false/TC-AF-4-pvl-inhale-false-gives-warning.pvl"
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-tests/cat5-assert-false/TC-AF-6-java-assume-false-gives-warning.java"
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-tests/cat5-assert-false/TC-AF-8-c-assume-false-gives-warning.c"
}
