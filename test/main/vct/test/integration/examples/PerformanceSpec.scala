package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class PerformanceSpec extends VercorsSpec {

  // ── Sequential loops ────────────────────────────────────────────────────────
  // Each file run three ways: all checkers on, checkInvSat off, detectDeadCode off.

  vercors should verify using silicon example "concepts/perf/seq-loops/PERF-SEQ-1loop.pvl"
  vercors should verify using silicon flags("--dev-no-loop-inv-sat") example "concepts/perf/seq-loops/PERF-SEQ-1loop.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/seq-loops/PERF-SEQ-1loop.pvl"

  vercors should verify using silicon example "concepts/perf/seq-loops/PERF-SEQ-5loops.pvl"
  vercors should verify using silicon flags("--dev-no-loop-inv-sat") example "concepts/perf/seq-loops/PERF-SEQ-5loops.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/seq-loops/PERF-SEQ-5loops.pvl"

  vercors should verify using silicon example "concepts/perf/seq-loops/PERF-SEQ-15loops.pvl"
  vercors should verify using silicon flags("--dev-no-loop-inv-sat") example "concepts/perf/seq-loops/PERF-SEQ-15loops.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/seq-loops/PERF-SEQ-15loops.pvl"

  vercors should verify using silicon example "concepts/perf/seq-loops/PERF-SEQ-5loops-indep.pvl"
  vercors should verify using silicon flags("--dev-no-loop-inv-sat") example "concepts/perf/seq-loops/PERF-SEQ-5loops-indep.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/seq-loops/PERF-SEQ-5loops-indep.pvl"

  // ── Nested loops ─────────────────────────────────────────────────────────────
  // Same three-way run, with 2/3/5 levels of nesting.

  vercors should verify using silicon example "concepts/perf/nest-loops/PERF-NEST-2level.pvl"
  vercors should verify using silicon flags("--dev-no-loop-inv-sat") example "concepts/perf/nest-loops/PERF-NEST-2level.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/nest-loops/PERF-NEST-2level.pvl"

  vercors should verify using silicon example "concepts/perf/nest-loops/PERF-NEST-3level.pvl"
  vercors should verify using silicon flags("--dev-no-loop-inv-sat") example "concepts/perf/nest-loops/PERF-NEST-3level.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/nest-loops/PERF-NEST-3level.pvl"

  vercors should verify using silicon example "concepts/perf/nest-loops/PERF-NEST-5level.pvl"
  vercors should verify using silicon flags("--dev-no-loop-inv-sat") example "concepts/perf/nest-loops/PERF-NEST-5level.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/nest-loops/PERF-NEST-5level.pvl"

  // ── Invariant complexity ─────────────────────────────────────────────────────
  // Fixed structure (1 loop), checkInvSat isolated; light/heavy = minimal vs. scaled-up invariant.
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-arithmetic-light.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-arithmetic.pvl"

  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-perm-light.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-perm-1field.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-perm-5fields.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-perm-5objects.pvl"

  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-forall-light.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-forall.pvl"

  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-funcall-d1-light.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-funcall-d1.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-funcall-d3.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-combined.pvl"

  // ── Branches ─────────────────────────────────────────────────────────────────
  // Each file run twice (all checkers on vs. --dev-no-dead) to isolate dead-code cost.

  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-5.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-5.pvl"

  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-15.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-15.pvl"

  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-30.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-30.pvl"

  // Same 30 conditions as PERF-BRANCH-30, but the else branch is removed (30 Refutes instead of 60).
  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-no-else-30.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-no-else-30.pvl"

  // Chained if/else-if/.../else: accumulated negations over multi-variable conditions.
  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-chain-5.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-chain-5.pvl"

  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-chain-10.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-chain-10.pvl"

  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-chain-15.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-chain-15.pvl"

  // 30-way chain, all branches live (29 accumulated multi-variable negations in the final else).
  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-chain-30.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-chain-30.pvl"

  // Same chain shape, but every condition is on its own independent variable.
  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-chain-indep-5.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-chain-indep-5.pvl"

  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-chain-indep-10.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-chain-indep-10.pvl"

  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-chain-indep-12.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-chain-indep-12.pvl"

  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-chain-indep-30.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-chain-indep-30.pvl"

  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-chain-indep-50.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-chain-indep-50.pvl"

  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-indep-5.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-indep-5.pvl"

  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-indep-10.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-indep-10.pvl"

  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-indep-12.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-indep-12.pvl"

  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-indep-13.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-indep-13.pvl"

  // 14: near the practical ceiling for this series (282s standalone).
  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-indep-14.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-indep-14.pvl"

  // indep-15/20/30 intentionally not run: they exceed the 900s test timeout (see each file).

  // Same shape as PERF-BRANCH-indep-30 (30 sequential ifs, no else), but every
  // condition is on a SINGLE shared variable `a` with increasing thresholds.
  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-single-var-30.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-single-var-30.pvl"

  // Fully nested if-else, 5 levels deep (62 Refutes) — compare to nest-7 for depth scaling.
  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-nest-5.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-nest-5.pvl"

  // Fully nested if-else, 7 levels deep (254 Refutes, innermost carries 7 stacked conditions).
  vercors should verify using silicon example "concepts/perf/branches/PERF-BRANCH-nest-7.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/branches/PERF-BRANCH-nest-7.pvl"

  // Same 30 branches as PERF-BRANCH-30 but all conditions are literally `true`:
  // detectDeadCode must report all 30 dead branches.
  vercors should fail withCode "deadBranch" using silicon example "concepts/perf/branches/PERF-BRANCH-30-const.pvl"

  // ── Parallel blocks ───────────────────────────────────────────────────────────
  // Each run twice: all checkers on, then detectDeadCode off (baseline).

  vercors should verify using silicon example "concepts/perf/parallel/PERF-PAR-simple.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/parallel/PERF-PAR-simple.pvl"

  vercors should verify using silicon example "concepts/perf/parallel/PERF-PAR-4blocks.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/parallel/PERF-PAR-4blocks.pvl"

  // Same 4 blocks, but each thread's branches are on independent variables.
  vercors should verify using silicon example "concepts/perf/parallel/PERF-PAR-4blocks-indep.pvl"
  vercors should verify using silicon flags("--dev-no-dead") example "concepts/perf/parallel/PERF-PAR-4blocks-indep.pvl"

  // ── Block (parallel) invariant complexity ─────────────────────────────────────
  // Same flag isolation as loop invariants (--dev-no-loop-inv-sat covers both).

  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/block-inv/PERF-BLOCKINV-perm-1field.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/block-inv/PERF-BLOCKINV-perm-5fields.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/block-inv/PERF-BLOCKINV-perm-10fields.pvl"

  // ── Lock invariants ───────────────────────────────────────────────────────────
  // Lock checker has no disable flag — all other checkers disabled for isolation.

  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/locks/PERF-LOCK-perm-1field.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/locks/PERF-LOCK-perm-5fields.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/locks/PERF-LOCK-perm-10fields.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/locks/PERF-LOCK-perm-15fields.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/locks/PERF-LOCK-5classes.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/locks/PERF-LOCK-dedup-5sites.pvl"

  // ── Postconditions ────────────────────────────────────────────────────────────
  // checkPostSat only — disable all other checkers.

  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-loop-inv-sat") example "concepts/perf/postcond/PERF-POST-simple.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-loop-inv-sat") example "concepts/perf/postcond/PERF-POST-funcall-d3.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-loop-inv-sat") example "concepts/perf/postcond/PERF-POST-indep-5.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-loop-inv-sat") example "concepts/perf/postcond/PERF-POST-indep-5-1cond.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-loop-inv-sat") example "concepts/perf/postcond/PERF-POST-perm.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-loop-inv-sat") example "concepts/perf/postcond/PERF-POST-10methods.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-loop-inv-sat") example "concepts/perf/postcond/PERF-POST-30methods.pvl"

  // ── Preconditions ─────────────────────────────────────────────────────────────
  // checkSat (CheckContractSatisfiability) only — mirrors postcond but for requires.

  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/precond/PERF-PRECOND-simple.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/precond/PERF-PRECOND-funcall-d3.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/precond/PERF-PRECOND-perm.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/precond/PERF-PRECOND-10methods.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/precond/PERF-PRECOND-30methods.pvl"

  // ── Sat vs unsat pairs ────────────────────────────────────────────────────────
  // Each pair: same formula, one satisfiable (pass) and one contradictory (fail).

  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/sat-unsat/PERF-SAT-inv-arith.pvl"
  vercors should AnyFail using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/sat-unsat/PERF-UNSAT-inv-arith.pvl"

  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/sat-unsat/PERF-SAT-inv-perm.pvl"
  vercors should AnyFail using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/sat-unsat/PERF-UNSAT-inv-perm.pvl"

  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-loop-inv-sat") example "concepts/perf/sat-unsat/PERF-SAT-post.pvl"
  vercors should AnyFail using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-loop-inv-sat") example "concepts/perf/sat-unsat/PERF-UNSAT-post.pvl"

  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/sat-unsat/PERF-SAT-lock.pvl"
  vercors should AnyFail using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/sat-unsat/PERF-UNSAT-lock.pvl"

  // ── Real-life combined programs ───────────────────────────────────────────────
  // Each program hits multiple checkers at once; run all-on and all-off to expose interaction cost.

  // Event processor: 5-field lock + forall loop invariant + 5-way chain branch.
  vercors should verify using silicon example "concepts/perf/combined/PERF-REAL-event-processor.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat", "--dev-no-loop-inv-sat", "--dev-no-lock-inv-sat") example "concepts/perf/combined/PERF-REAL-event-processor.pvl"

  // Heavier event processor: 15-field lock/loop invariants, independent (non-chained) branches.
  vercors should verify using silicon example "concepts/perf/combined/PERF-REAL-event-processor-heavy.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat", "--dev-no-loop-inv-sat", "--dev-no-lock-inv-sat") example "concepts/perf/combined/PERF-REAL-event-processor-heavy.pvl"

  // Analytics: 3 sequential forall loops each with 3-way branch + postcondition.
  vercors should verify using silicon example "concepts/perf/combined/PERF-REAL-analytics.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat", "--dev-no-loop-inv-sat", "--dev-no-lock-inv-sat") example "concepts/perf/combined/PERF-REAL-analytics.pvl"

  // Parallel pipeline: 2-field lock class + forall prep loop + 4 par blocks x 4-way branches.
  vercors should verify using silicon example "concepts/perf/combined/PERF-REAL-parallel-pipeline.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat", "--dev-no-loop-inv-sat", "--dev-no-lock-inv-sat") example "concepts/perf/combined/PERF-REAL-parallel-pipeline.pvl"

  // ── Real-life programs (full example suite) ──────────────────────────────────
  // Each run three ways: baseline, +dead-code detection, +all checkers.
  // challenge1.pvl — 39 loops.
  vercors should verify using silicon flags ("--dev-no-dead", "--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat") example "verifythis/2019/challenge1.pvl"
  vercors should verify using silicon flags ("--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat") example "verifythis/2019/challenge1.pvl"
  vercors should verify using silicon example "verifythis/2019/challenge1.pvl"

  // KahnsTopologicalSort.pvl — 61 loops (most loop-dense).
  vercors should verify using silicon flags ("--dev-no-dead", "--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat") example "concepts/algo/KahnsTopologicalSort.pvl"
  vercors should verify using silicon flags ("--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat") example "concepts/algo/KahnsTopologicalSort.pvl"
  vercors should verify using silicon example "concepts/algo/KahnsTopologicalSort.pvl"

  // relaxed_prefix.pvl — 35 loops.
  vercors should verify using silicon flags ("--dev-no-dead", "--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat") example "verifythis/2015/relaxed_prefix.pvl"
  vercors should verify using silicon flags ("--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat") example "verifythis/2015/relaxed_prefix.pvl"
  vercors should verify using silicon example "verifythis/2015/relaxed_prefix.pvl"

  // challenge3_complete.pvl — 27 branches.
  vercors should verify using silicon flags ("--dev-no-dead", "--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat", "--no-infer-heap-context-into-frame") example "verifythis/2019/challenge3_complete.pvl"
  vercors should verify using silicon flags ("--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat", "--no-infer-heap-context-into-frame") example "verifythis/2019/challenge3_complete.pvl"
  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "verifythis/2019/challenge3_complete.pvl"

  // ArrayList.java — 36 loops, many methods.
  vercors should verify using silicon flags ("--dev-no-dead", "--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat") example "concepts/arrays/ArrayList.java"
  vercors should verify using silicon flags ("--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat") example "concepts/arrays/ArrayList.java"
  vercors should verify using silicon example "concepts/arrays/ArrayList.java"
}
