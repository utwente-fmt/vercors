package integration

import hre.util.Verdict
import integration.helper.{IntegrationTestConfiguration, IntegrationTestHelper}
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Millis, Span}


/*+
  These tests have been generated based on the old test framework. For new test I would recommend splitting it up in
  different classes and giving more descriptive names.
 */

class GeneratedSlow2Tests extends AnyFlatSpec with TimeLimitedTests with Matchers {

  override def timeLimit: Span = Span(60000,Millis)

  "VerCors" should "pass with silicon and examples/dafny/DafnyIncr.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/dafny/DafnyIncr.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/demo/demo2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/demo/demo2.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/demo/demo3a.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/demo/demo3a.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/demo/demo3b.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/demo/demo3b.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/demo/demo3c.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/demo/demo3c.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/fixed-known-problems/rewriterIssue5.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/fixed-known-problems/rewriterIssue5.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/fixed-known-problems/rewriterIssue7.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/fixed-known-problems/rewriterIssue7.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/floats/TestCountFail.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/floats/TestCountFail.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/floats/TestCountPass.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/floats/TestCountPass.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/forkjoin/forkjoininforloop.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/forkjoin/forkjoininforloop.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/futures/TestFuturePermsFail.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/futures/TestFuturePermsFail.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/futures/locking.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/futures/locking.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/goto/LabeledWhile.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/LabeledWhile.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with carbon and examples/goto/LabeledWhile.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/LabeledWhile.java")
    configuration.verdict = Verdict.Pass
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/gpgpu/simple-ltid.cu" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/gpgpu/simple-ltid.cu")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/histories/History.pvl with check defined" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/histories/History.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkDefined = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/histories/History.pvl with check Axioms" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/histories/History.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkAxioms = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/OwickiGries.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/OwickiGries.pvl", "examples/manual/Worker.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/functions.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/functions.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/list.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/list.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/loop.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/loop.pvl", "examples/manual/permissions.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/manual/main.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/main.pvl", "examples/manual/permissions.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/manual/permissions.pvl and examples/manual/parameters1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/permissions.pvl", "examples/manual/parameters1.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/permissions.pvl and examples/manual/parameters2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/permissions.pvl", "examples/manual/parameters2.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/zero_array.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/zero_array.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/maps/maps_example_from_dafny.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/maps/maps_example_from_dafny.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }
  it should "pass with silicon and examples/openmp/copy-spec.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/openmp/copy-spec.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/openmp/zero-spec.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/openmp/zero-spec.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/parallel/InvariantParallelAtomic.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/InvariantParallelAtomic.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/parallel/ParBothRead.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/ParBothRead.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/parallel/ParNestedInvariant.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/ParNestedInvariant.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/parallel/block-par.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/block-par.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/parallel/inv-test-fail1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/inv-test-fail1.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/parallel/inv-test-fail2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/inv-test-fail2.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/permissions/BadLoop1.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/BadLoop1.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/permissions/BadLoop2.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/BadLoop2.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/permissions/Counter.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/Counter.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/permissions/IncrFail.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/IncrFail.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/permissions/IncrPass.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/IncrPass.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/permissions/MultiIncrement.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/MultiIncrement.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/permissions/SwapIntegerFail.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/SwapIntegerFail.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/permissions/SwapIntegerPass.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/SwapIntegerPass.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/predicates/ScaleInlinePredicate.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/predicates/ScaleInlinePredicate.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/predicates/minmax-list.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/predicates/minmax-list.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequences/append-prepend-sequence.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequences/append-prepend-sequence.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequences/remove-value.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequences/remove-value.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequences/sequence-constructors.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequences/sequence-constructors.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequences/slicing.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequences/slicing.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/sequential/BoogieExampleFail.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequential/BoogieExampleFail.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequential/BoogieTest.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequential/BoogieTest.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sets/set_comprehension.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sets/set_comprehension.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sets/subset.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sets/subset.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/witnesses/WandDemoSilver.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/witnesses/WandDemoSilver.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/WhileBreakFinally.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/WhileBreakFinally.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/classes/NewException.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/classes/NewException.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/fixed-known-problems/rewriterIssue4.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/fixed-known-problems/rewriterIssue4.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/FinallyEmpty.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/FinallyEmpty.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }
}
