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

  it should "fail with silicon and examples/abrupt/ContinueBreakFail.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/ContinueBreakFail.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/clang/c-example-impl.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/clang/c-example-impl.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/demo/demo1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/demo/demo1.pvl")
    configuration.verdict = Verdict.Fail
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

  it should "pass with silicon and examples/arrays/array-example.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/array-example.pvl")
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

  it should "fail with silicon and examples/arrays/zero-array-ic-e1.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/arrays/zero-array-ic-e1.c")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/ContinueBreakPass.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/ContinueBreakPass.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/Finally.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/Finally.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/abrupt/OnlyReturn.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/OnlyReturn.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/carp/ZeroArrayIC.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/carp/ZeroArrayIC.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/carp/access-sub-matrix-fail.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/carp/access-sub-matrix-fail.c")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/carp/access-sub-matrix-pass.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/carp/access-sub-matrix-pass.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/carp/zero-kernel.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/carp/zero-kernel.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/carp/zero-loop.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/carp/zero-loop.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/carp/zero-sub-array.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/carp/zero-sub-array.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchEmpty.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchEmpty.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchIf.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchIf.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchLog.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchLog.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchNestedTry.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchNestedTry.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchStackTrace.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchStackTrace.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchSwitch.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchSwitch.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchThrowE.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchThrowE.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchThrowNewE.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchThrowNewE.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchThrowNewEWrap.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchThrowNewEWrap.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/CatchWhile.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/CatchWhile.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/FinallyNestedTry.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/FinallyNestedTry.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/FinallyReturn.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/FinallyReturn.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/FinallyThrowNewE.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/FinallyThrowNewE.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/clang/c-example-use.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/clang/c-example-use.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.progress = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/classes/DifferentClassesDifferentTypes1.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/classes/DifferentClassesDifferentTypes1.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/classes/DifferentClassesDifferentTypes2.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/classes/DifferentClassesDifferentTypes2.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/classes/NewRuntimeException.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/classes/NewRuntimeException.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/classes/Overloading.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/classes/Overloading.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/forkjoin/forkfail.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/forkjoin/forkfail.pvl")
    configuration.verdict = Verdict.Fail
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

  it should "pass with silicon and examples/sequential/nested-loops.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/sequential/nested-loops.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }
}
