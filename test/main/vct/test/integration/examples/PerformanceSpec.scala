package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class PerformanceSpec extends VercorsSpec {

  // ── Sequential loops ────────────────────────────────────────────────────────
  // Each file run twice: all checkers on (default), then checkInvSat only.
  // checkInvSat only = disable detectDeadCode + checkSat + checkPostSat.
  // detectDeadCode only = disable checkInvSat + checkSat + checkPostSat.

  vercors should verify using silicon example "concepts/perf/seq-loops/PERF-SEQ-1loop.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/seq-loops/PERF-SEQ-1loop.pvl"
  vercors should verify using silicon flags("--dev-no-loop-inv-sat", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/seq-loops/PERF-SEQ-1loop.pvl"

  vercors should verify using silicon example "concepts/perf/seq-loops/PERF-SEQ-5loops.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/seq-loops/PERF-SEQ-5loops.pvl"
  vercors should verify using silicon flags("--dev-no-loop-inv-sat", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/seq-loops/PERF-SEQ-5loops.pvl"

  vercors should verify using silicon example "concepts/perf/seq-loops/PERF-SEQ-15loops.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/seq-loops/PERF-SEQ-15loops.pvl"
  vercors should verify using silicon flags("--dev-no-loop-inv-sat", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/seq-loops/PERF-SEQ-15loops.pvl"

  // ── Nested loops ─────────────────────────────────────────────────────────────
  // Key experiment: checkInvSat should be same as sequential (isolated FramedProofs);
  // detectDeadCode should diverge (accumulated path conditions per nesting level).

  vercors should verify using silicon example "concepts/perf/nest-loops/PERF-NEST-2level.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/nest-loops/PERF-NEST-2level.pvl"
  vercors should verify using silicon flags("--dev-no-loop-inv-sat", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/nest-loops/PERF-NEST-2level.pvl"

  vercors should verify using silicon example "concepts/perf/nest-loops/PERF-NEST-3level.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/nest-loops/PERF-NEST-3level.pvl"
  vercors should verify using silicon flags("--dev-no-loop-inv-sat", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/nest-loops/PERF-NEST-3level.pvl"

  vercors should verify using silicon example "concepts/perf/nest-loops/PERF-NEST-5level.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/nest-loops/PERF-NEST-5level.pvl"
  vercors should verify using silicon flags("--dev-no-loop-inv-sat", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/nest-loops/PERF-NEST-5level.pvl"

  // ── Invariant complexity ─────────────────────────────────────────────────────
  // Fixed structure (1 loop), vary what is inside the invariant.
  // All run with checkInvSat only to isolate the per-check formula cost.

  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-arithmetic.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-perm-1field.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-perm-5fields.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-forall.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-funcall-d1.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-funcall-d3.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/inv-complexity/PERF-INV-combined.pvl"

  // ── Branches ─────────────────────────────────────────────────────────────────
  // detectDeadCode only — no loop invariant or postcondition checker noise.

  vercors should verify using silicon flags("--dev-no-loop-inv-sat", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/branches/PERF-BRANCH-5.pvl"
  vercors should verify using silicon flags("--dev-no-loop-inv-sat", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/branches/PERF-BRANCH-15.pvl"
  vercors should verify using silicon flags("--dev-no-loop-inv-sat", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/branches/PERF-BRANCH-30.pvl"

  // Chained if/else-if/.../else: each branch carries accumulated negations of prior conditions.
  // Compare to sequential pairs above — chains accumulate; sequential pairs are independent.
  vercors should verify using silicon flags("--dev-no-loop-inv-sat", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/branches/PERF-BRANCH-chain-5.pvl"
  vercors should verify using silicon flags("--dev-no-loop-inv-sat", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/branches/PERF-BRANCH-chain-10.pvl"
  vercors should verify using silicon flags("--dev-no-loop-inv-sat", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/branches/PERF-BRANCH-chain-15.pvl"

  // ── Parallel blocks ───────────────────────────────────────────────────────────
  // detectDeadCode only — first performance data on parallel block instrumentation.

  vercors should verify using silicon flags("--dev-no-loop-inv-sat", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/parallel/PERF-PAR-simple.pvl"
  vercors should verify using silicon flags("--dev-no-loop-inv-sat", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/parallel/PERF-PAR-4blocks.pvl"

  // ── Lock invariants ───────────────────────────────────────────────────────────
  // Lock checker has no disable flag — always runs. All other checkers disabled for isolation.

  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/locks/PERF-LOCK-perm-1field.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/locks/PERF-LOCK-perm-5fields.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/locks/PERF-LOCK-5classes.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/locks/PERF-LOCK-dedup-5sites.pvl"

  // ── Postconditions ────────────────────────────────────────────────────────────
  // checkPostSat only — disable all other checkers.

  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-loop-inv-sat") example "concepts/perf/postcond/PERF-POST-simple.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-loop-inv-sat") example "concepts/perf/postcond/PERF-POST-funcall-d3.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-loop-inv-sat") example "concepts/perf/postcond/PERF-POST-old-forall.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-loop-inv-sat") example "concepts/perf/postcond/PERF-POST-perm.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-loop-inv-sat") example "concepts/perf/postcond/PERF-POST-10methods.pvl"
  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-loop-inv-sat") example "concepts/perf/postcond/PERF-POST-30methods.pvl"

  // ── Sat vs unsat pairs ────────────────────────────────────────────────────────
  // Each pair: same formula, one satisfiable (pass) and one contradictory (fail).

  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/sat-unsat/PERF-SAT-inv-arith.pvl"
  vercors should fail using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/sat-unsat/PERF-UNSAT-inv-arith.pvl"

  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/sat-unsat/PERF-SAT-inv-perm.pvl"
  vercors should fail using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat") example "concepts/perf/sat-unsat/PERF-UNSAT-inv-perm.pvl"

  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-loop-inv-sat") example "concepts/perf/sat-unsat/PERF-SAT-post.pvl"
  vercors should fail using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-loop-inv-sat") example "concepts/perf/sat-unsat/PERF-UNSAT-post.pvl"

  vercors should verify using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/sat-unsat/PERF-SAT-lock.pvl"
  vercors should fail using silicon flags("--dev-no-dead", "--dev-no-sat", "--dev-no-post-sat", "--dev-no-loop-inv-sat") example "concepts/perf/sat-unsat/PERF-UNSAT-lock.pvl"

  // ── All flags combined ────────────────────────────────────────────────────────
  // All five checkers active simultaneously — tests additive vs interference cost.

  vercors should verify using silicon example "concepts/perf/combined/PERF-COMBINED-all.pvl"
}
