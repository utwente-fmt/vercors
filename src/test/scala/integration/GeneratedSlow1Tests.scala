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

class GeneratedSlow1Tests extends AnyFlatSpec with TimeLimitedTests with Matchers {

  override def timeLimit: Span = Span(60000,Millis)

  "VerCors" should "pass with silicon and examples/abrupt/Abrupt.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/Abrupt.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/DoubleNestedFinally.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/DoubleNestedFinally.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/ExceptionAbortsAssignment.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/ExceptionAbortsAssignment.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/ExceptionsAndAbrupt.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/ExceptionsAndAbrupt.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/GoodCustomException.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/GoodCustomException.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with carbon and examples/abrupt/KeYAbruptTerminationChallengeCarbon.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/KeYAbruptTerminationChallengeCarbon.java")
    configuration.verdict = Verdict.Pass
    configuration.toolCarbon= true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/abrupt/KeYAbruptTerminationChallengeSilicon.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/KeYAbruptTerminationChallengeSilicon.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/KnuthTabulate.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/KnuthTabulate.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/LabeledStatements.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/LabeledStatements.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/NestedTryCatchFinally.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/NestedTryCatchFinally.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/OnlyCatch.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/OnlyCatch.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/OnlyThrows.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/OnlyThrows.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/OverloadedReturn.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/OverloadedReturn.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/ReturnFinally.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/ReturnFinally.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/abrupt/SignalsMustPropagate.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/SignalsMustPropagate.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/SignalsThrowNoThrows.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/SignalsThrowNoThrows.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/SwitchVarious.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/SwitchVarious.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/Synchronized.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/Synchronized.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/ThrowsAndThrow.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/ThrowsAndThrow.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/TypecheckSignalsOk.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/TypecheckSignalsOk.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/TypecheckTryCatch.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/TypecheckTryCatch.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/WhileFinally.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/WhileFinally.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/WhileInFinally.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/WhileInFinally.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/BinarySearch.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/BinarySearch.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/DutchNationalFlag.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/DutchNationalFlag.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/JavaArrayExamples.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/JavaArrayExamples.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/Transpose.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/Transpose.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/array-example.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/array-example.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/arrays/backward-dep-e1.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/backward-dep-e1.c")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/basic-examples.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/basic-examples.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/arrays/forward-dep-e1.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/forward-dep-e1.c")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/forward-dep-noauto.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/forward-dep-noauto.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/forward-dep.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/forward-dep.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/kernel-example-v2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/kernel-example-v2.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/kernel-example-v3.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/kernel-example-v3.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/kernel-example.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/kernel-example.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/arrays/vector-add.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/vector-add.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/arrays/zero-array-ic-e1.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/zero-array-ic-e1.c")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }
}
