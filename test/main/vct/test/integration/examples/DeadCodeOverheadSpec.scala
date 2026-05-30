package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

// Benchmark suite for measuring overhead of smoke-testing passes.
//
// Each program is run in three variants — the auto-generated test name includes
// the flags, so the output looks like:
//   - Examples .../challenge1.pvl with flags --dev-no-dead ...  (baseline)
//   - Examples .../challenge1.pvl with flags --dev-no-loop-inv-sat ...  (+dead)
//   - Examples .../challenge1.pvl  (+all)
//
// Run with -oD to get per-test durations:
//   java -Xss128m -Xmx4G -cp out.jar:res/universal/deps:res/universal/res \
//     org.scalatest.tools.Runner -oD \
//     -s vct.test.integration.examples.DeadCodeOverheadSpec
//
// (c) - (a) = total smoke overhead; (b) - (a) = dead-code detection cost alone
class DeadCodeOverheadSpec extends VercorsSpec {

  // ── verifythis/2019/challenge1.pvl — 39 loops ───────────────────────────────
  vercors should verify using silicon flags ("--dev-no-dead", "--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat") example "verifythis/2019/challenge1.pvl"
  vercors should verify using silicon flags ("--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat") example "verifythis/2019/challenge1.pvl"
  vercors should verify using silicon example "verifythis/2019/challenge1.pvl"

  // ── concepts/algo/KahnsTopologicalSort.pvl — 61 loops (most loop-dense) ─────
  vercors should verify using silicon flags ("--dev-no-dead", "--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat") example "concepts/algo/KahnsTopologicalSort.pvl"
  vercors should verify using silicon flags ("--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat") example "concepts/algo/KahnsTopologicalSort.pvl"
  vercors should verify using silicon example "concepts/algo/KahnsTopologicalSort.pvl"

  // ── verifythis/2015/relaxed_prefix.pvl — 35 loops ───────────────────────────
  vercors should verify using silicon flags ("--dev-no-dead", "--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat") example "verifythis/2015/relaxed_prefix.pvl"
  vercors should verify using silicon flags ("--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat") example "verifythis/2015/relaxed_prefix.pvl"
  vercors should verify using silicon example "verifythis/2015/relaxed_prefix.pvl"

  // ── verifythis/2019/challenge3_complete.pvl — 27 branches ───────────────────
  vercors should verify using silicon flags ("--dev-no-dead", "--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat", "--no-infer-heap-context-into-frame") example "verifythis/2019/challenge3_complete.pvl"
  vercors should verify using silicon flags ("--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat", "--no-infer-heap-context-into-frame") example "verifythis/2019/challenge3_complete.pvl"
  vercors should verify using silicon flag "--no-infer-heap-context-into-frame" example "verifythis/2019/challenge3_complete.pvl"

  // ── concepts/arrays/ArrayList.java — 36 loops, many methods ─────────────────
  vercors should verify using silicon flags ("--dev-no-dead", "--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat") example "concepts/arrays/ArrayList.java"
  vercors should verify using silicon flags ("--dev-no-loop-inv-sat", "--dev-no-post-sat", "--dev-no-sat") example "concepts/arrays/ArrayList.java"
  vercors should verify using silicon example "concepts/arrays/ArrayList.java"
}
