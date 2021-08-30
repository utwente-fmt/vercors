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

class Generated2Tests extends AnyFlatSpec with TimeLimitedTests with Matchers {

  override def timeLimit: Span = Span(5000,Millis)


  it should "fail with silicon and examples/forkjoin/forkfail.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/forkjoin/forkfail.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/forkjoin/forkpass.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/forkjoin/forkpass.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/goto/BadLabeledIfWithContract.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/BadLabeledIfWithContract.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/goto/LabeledIf.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/LabeledIf.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with carbon and examples/goto/LabeledIf.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/LabeledIf.java")
    configuration.verdict = Verdict.Pass
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/goto/goto1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/goto1.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with carbon and examples/goto/goto1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/goto1.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/goto/goto2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/goto2.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with carbon and examples/goto/goto2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/goto2.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/goto/goto3.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/goto3.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with carbon and examples/goto/goto3.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/goto/goto3.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/manual/BadType.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/BadType.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/manual/BadType.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/BadType.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/manual/TestFork.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/TestFork.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/induction-lemma.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/induction-lemma.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/manual/induction-problem.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/induction-problem.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/option.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/option.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/manual/permissions.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/manual/permissions.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/parallel/ForWithinParallel.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/ForWithinParallel.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/parallel/ParBothWrite.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/ParBothWrite.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/parallel/ParIterWrite.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/ParIterWrite.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/parallel/ParNestedInvariantWrite.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/parallel/ParNestedInvariantWrite.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/permissions/box.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/permissions/box.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/predicates/MutuallyRecursiveInlinePredicates.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/predicates/MutuallyRecursiveInlinePredicates.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/predicates/RecursiveInlinePredicate.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/predicates/RecursiveInlinePredicate.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/refute/Sat.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/refute/Sat.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/refute/Unsat.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/refute/Unsat.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/refute/frame_error_1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/refute/frame_error_1.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/refute/refute1.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/refute/refute1.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/refute/refute3.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/refute/refute3.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/refute/refute4.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/refute/refute4.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/refute/refute5.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/refute/refute5.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequences/updating.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequences/updating.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequential/BoogieExamplePass.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequential/BoogieExamplePass.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/sequential/BoogieTestFail.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequential/BoogieTestFail.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/sequential/LoopInvFail.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequential/LoopInvFail.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequential/LoopInvPass.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequential/LoopInvPass.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequential/SimpleExamples.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequential/SimpleExamples.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/sequential/nested-loops.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequential/nested-loops.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/technical/keywords/allowed-c.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/keywords/allowed-c.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with carbon and examples/technical/keywords/allowed-c.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/keywords/allowed-c.c")
    configuration.verdict = Verdict.Pass
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/technical/keywords/allowed-java.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/keywords/allowed-java.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with carbon and examples/technical/keywords/allowed-java.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/keywords/allowed-java.java")
    configuration.verdict = Verdict.Pass
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/technical/keywords/disallowed-c-inline.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/keywords/disallowed-c-inline.c")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with carbon and examples/technical/keywords/disallowed-c-inline.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/keywords/disallowed-c-inline.c")
    configuration.verdict = Verdict.Error
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/technical/keywords/disallowed-java-assert.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/keywords/disallowed-java-assert.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with carbon and examples/technical/keywords/disallowed-java-assert.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/keywords/disallowed-java-assert.java")
    configuration.verdict = Verdict.Error
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/technical/test-value1.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/test-value1.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/technical/test-value2.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/technical/test-value2.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/type-casts/TypeExample1.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/type-casts/TypeExample1.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/type-casts/cast-example.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/type-casts/cast-example.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

}
