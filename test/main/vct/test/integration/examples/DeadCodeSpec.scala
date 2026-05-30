package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class DeadCodeSpec extends VercorsSpec {
  // Dead branches — expect a deadBranch failure
  vercors should fail withCode "deadBranch" using silicon example "smoke_tests/dead-code-branch-dead.java"
  vercors should fail withCode "deadBranch" using silicon example "smoke_tests/dead-code-else-dead.java"
  vercors should fail withCode "deadBranch" using silicon example "smoke_tests/dead-code-loop-dead.java"

  // Dead nested branch only detectable with accumulated path conditions
  vercors should fail withCode "deadBranch" using silicon example "smoke_tests/dead-code-nested-dead.java"

  // Dead branch in pure method (Select expression) — expect a deadBranch failure
  vercors should fail withCode "deadBranch" using silicon example "smoke_tests/dead-code-pure-method-dead.java"

  // Dead branch where precondition is expressed via a pure function
  vercors should fail withCode "deadBranch" using silicon example "smoke_tests/dead-code-pure-spec-dead.java"

  // Live branches and loops — expect clean verification, no deadBranch warnings
  vercors should verify using silicon example "smoke_tests/dead-code-branch-live.java"
  vercors should verify using silicon example "smoke_tests/dead-code-loop-live.java"
  vercors should verify using silicon example "smoke_tests/dead-code-pure-method-live.java"

  // PVL (InstanceMethod via PVL front-end)
  vercors should fail withCode "deadBranch" using silicon example "smoke_tests/dead-code-branch-dead.pvl"
  vercors should fail withCode "deadBranch" using silicon example "smoke_tests/dead-code-loop-dead.pvl"
  vercors should verify using silicon example "smoke_tests/dead-code-branch-live.pvl"

  // C (Procedure in COL — C functions have no implicit this)
  vercors should fail withCode "deadBranch" using silicon example "smoke_tests/dead-code-branch-dead.c"
  vercors should fail withCode "deadBranch" using silicon example "smoke_tests/dead-code-loop-dead.c"
  vercors should verify using silicon example "smoke_tests/dead-code-branch-live.c"

  // Cascade suppression: dead outer branch — inner branches should not each produce a separate error
  vercors should fail withCode "deadBranch" using silicon example "smoke_tests/dead-code-cascade.java"

  // Postcondition satisfiability (CheckPostconditionSatisfiability)
  // The checker tests only the postcondition in isolation — the precondition applies to a
  // different program state (pre-call) and the method body may freely transform state,
  // so combining requires ∧ ensures would yield false positives.

  // Pass (PVL): satisfiable postconditions — checker must not fire
  vercors should verify using silicon example "concepts/smoke_dead_post/cat2-7-postcondition/TC-PST-1-ensures-satisfiable.pvl"
  vercors should verify using silicon example "concepts/smoke_dead_post/cat2-7-postcondition/TC-PST-3-ensures-false-intentional.pvl"
  vercors should verify using silicon example "concepts/smoke_dead_post/cat2-7-postcondition/TC-PST-6-ensures-stricter-satisfiable.pvl"
  vercors should verify using silicon example "concepts/smoke_dead_post/cat2-7-postcondition/TC-PST-7-ensures-satisfiable-void.pvl"
  vercors should verify using silicon example "concepts/smoke_dead_post/cat2-7-postcondition/TC-PST-8-ensures-result-range-sat.pvl"
  vercors should verify using silicon example "concepts/smoke_dead_post/cat2-7-postcondition/TC-PST-10-requires-narrows-ensures-sat.pvl"
  // PST-11: precondition conflicts with postcondition for the same variable, but the
  // postcondition is satisfiable on its own and the body establishes it — must not fire.
  vercors should verify using silicon example "concepts/smoke_dead_post/cat2-7-postcondition/TC-PST-11-requires-conflicts-ensures.pvl"

  // Fail (PVL): postcondition contradicts itself — checker must fire with postUnsatisfiable
  vercors should fail withCode "postUnsatisfiable" using silicon example "concepts/smoke_dead_post/cat2-7-postcondition/TC-PST-2-ensures-unsat-contradiction.pvl"
  vercors should fail withCode "postUnsatisfiable" using silicon example "concepts/smoke_dead_post/cat2-7-postcondition/TC-PST-4-ensures-result-contradiction.pvl"
  vercors should fail withCode "postUnsatisfiable" using silicon example "concepts/smoke_dead_post/cat2-7-postcondition/TC-PST-5-ensures-perm-false.pvl"
  vercors should fail withCode "postUnsatisfiable" using silicon example "concepts/smoke_dead_post/cat2-7-postcondition/TC-PST-9-ensures-n-gt-n.pvl"

  // Pass (Java): satisfiable postconditions — checker must not fire
  vercors should verify using silicon example "concepts/smoke_dead_post/cat2-7-postcondition/TC-PST-12-java-ensures-satisfiable.java"
  vercors should verify using silicon example "concepts/smoke_dead_post/cat2-7-postcondition/TC-PST-14-java-requires-conflicts-ensures.java"

  // Fail (Java): postcondition contradicts itself — checker must fire with postUnsatisfiable
  vercors should fail withCode "postUnsatisfiable" using silicon example "concepts/smoke_dead_post/cat2-7-postcondition/TC-PST-13-java-ensures-unsat.java"

  // Pass (C): satisfiable postconditions — checker must not fire
  vercors should verify using silicon example "concepts/smoke_dead_post/cat2-7-postcondition/TC-PST-15-c-ensures-satisfiable.c"

  // Fail (C): postcondition contradicts itself — checker must fire with postUnsatisfiable
  vercors should fail withCode "postUnsatisfiable" using silicon example "concepts/smoke_dead_post/cat2-7-postcondition/TC-PST-16-c-ensures-unsat.c"

  // Loop invariant + post-loop dead code (DetectDeadCode)
  // Dead if-branch after loop: invariant+exit makes branch condition impossible
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-dead/cat2-4-loop-invariant/TC-LI-1-post-loop-dead.pvl"
  // Negative: post-loop branch is live — no error
  vercors should verify using silicon example "concepts/smoke-test-dead/cat2-4-loop-invariant/TC-LI-4-negative-consistent.pvl"
  // Post-loop state false: invariant implies !condition, code after loop is unreachable
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-dead/cat2-4-loop-invariant/TC-LI-6-post-loop-sequential-dead.pvl"
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-dead/cat2-4-loop-invariant/TC-LI-7-post-loop-invariant-stronger-than-condition.pvl"
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-dead/cat2-4-loop-invariant/TC-LI-8-post-loop-context-everywhere-dead.pvl"
  // Negative: loop genuinely terminates, post-loop state satisfiable
  vercors should verify using silicon example "concepts/smoke-test-dead/cat2-4-loop-invariant/TC-LI-10-post-loop-negative-terminates.pvl"

  // Loop invariant satisfiability (CheckLoopInvariantSatisfiability)
  // Pass (PVL): satisfiable invariants — checker must not fire
  vercors should verify using silicon example "concepts/smoke-test-dead/cat2-4-loop-invariant/TC-LI-11-invariant-satisfiable-pass.pvl"
  vercors should verify using silicon example "concepts/smoke-test-dead/cat2-4-loop-invariant/TC-LI-18-invariant-with-perm-satisfiable.pvl"

  // Fail (PVL): unsatisfiable invariants — checker must fire with invariantUnsatisfiable
  vercors should fail withCode "invariantUnsatisfiable" using silicon example "concepts/smoke-test-dead/cat2-4-loop-invariant/TC-LI-12-invariant-false-unsat.pvl"
  vercors should fail withCode "invariantUnsatisfiable" using silicon example "concepts/smoke-test-dead/cat2-4-loop-invariant/TC-LI-13-invariant-contradiction-unsat.pvl"
  vercors should fail withCode "invariantUnsatisfiable" using silicon example "concepts/smoke-test-dead/cat2-4-loop-invariant/TC-LI-19-nested-loop-both-checked.pvl"

  // Pass (Java): satisfiable invariant — checker must not fire
  vercors should verify using silicon example "concepts/smoke-test-dead/cat2-4-loop-invariant/TC-LI-14-java-invariant-satisfiable.java"

  // Fail (Java): unsatisfiable invariant — checker must fire with invariantUnsatisfiable
  vercors should fail withCode "invariantUnsatisfiable" using silicon example "concepts/smoke-test-dead/cat2-4-loop-invariant/TC-LI-15-java-invariant-contradiction-unsat.java"

  // Pass (C): satisfiable invariant — checker must not fire
  vercors should verify using silicon example "concepts/smoke-test-dead/cat2-4-loop-invariant/TC-LI-16-c-invariant-satisfiable.c"

  // Fail (C): unsatisfiable invariant — checker must fire with invariantUnsatisfiable
  vercors should fail withCode "invariantUnsatisfiable" using silicon example "concepts/smoke-test-dead/cat2-4-loop-invariant/TC-LI-17-c-invariant-contradiction-unsat.c"

  // Cascade suppression: all live loop with invariant established and maintained
  vercors should verify using silicon example "concepts/smoke-test-dead/cat3-cascade/TC-CA-13-negative-loop-all-live.pvl"

  // assert false as intentional dead code marker — block cutoff, assert itself fails
  vercors should fail withCode "assertFailed:false" using silicon example "concepts/smoke-test-dead/cat5-assert-false/TC-AF-1-pvl-assert-false-mid-block.pvl"
  vercors should fail withCode "assertFailed:false" using silicon example "concepts/smoke-test-dead/cat5-assert-false/TC-AF-2-pvl-assert-false-in-loop-body.pvl"
  vercors should fail withCode "assertFailed:false" using silicon example "concepts/smoke-test-dead/cat5-assert-false/TC-AF-5-java-assert-false-in-branch.java"
  vercors should fail withCode "assertFailed:false" using silicon example "concepts/smoke-test-dead/cat5-assert-false/TC-AF-7-c-assert-false-mid-block.c"

  // assume false and inhale false produce deadBranch warnings
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-dead/cat5-assert-false/TC-AF-3-pvl-assume-false-gives-warning.pvl"
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-dead/cat5-assert-false/TC-AF-4-pvl-inhale-false-gives-warning.pvl"
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-dead/cat5-assert-false/TC-AF-6-java-assume-false-gives-warning.java"
  vercors should fail withCode "deadBranch" using silicon example "concepts/smoke-test-dead/cat5-assert-false/TC-AF-8-c-assume-false-gives-warning.c"
}
