package integration

import hre.util.Verdict
import integration.helper.{IntegrationTestConfiguration, IntegrationTestHelper}
import org.scalatest.Ignore
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Millis, Span}


/*+
  These tests have been generated based on the old test framework. For new test I would recommend splitting it up in
  different classes and giving more descriptive names.
 */

class Generated1Tests extends AnyFlatSpec with TimeLimitedTests with Matchers {

  override def timeLimit: Span = Span(5000,Millis)

  it should "error with silicon and examples/abrupt/BadCustomException.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/BadCustomException.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    configuration.progress = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/CatchTypesIdenticalBad.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/CatchTypesIdenticalBad.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/CatchTypesSubtypeBad.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/CatchTypesSubtypeBad.java")
    configuration.verdict = Verdict.Error
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

  it should "error with silicon and examples/abrupt/NoResultInSignals.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/NoResultInSignals.java")
    configuration.verdict = Verdict.Error
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

  it should "error with silicon and examples/abrupt/PureSignalsBad.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/PureSignalsBad.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/PureThrowsBad.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/PureThrowsBad.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/SignalsMustExtendThrowable.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/SignalsMustExtendThrowable.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/ThrowIntBad.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/ThrowIntBad.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/TypecheckSignalsBad.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/TypecheckSignalsBad.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/TypecheckThrowsMethodBad.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/TypecheckThrowsMethodBad.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/TypecheckThrowsThrowBad.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/TypecheckThrowsThrowBad.java")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/abrupt/UnusedCheckedCatchBad.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/abrupt/UnusedCheckedCatchBad.java")
    configuration.verdict = Verdict.Error
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

  it should "pass with silicon and examples/case-studies/exception-patterns/FinallyEmpty.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/FinallyEmpty.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/case-studies/exception-patterns/FinallyLog.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/case-studies/exception-patterns/FinallyLog.java")
    configuration.verdict = Verdict.Pass
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

  it should "error with silicon and examples/classes/InnerClass.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/classes/InnerClass.java")
    configuration.verdict = Verdict.Error
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

  it should "pass with silicon and examples/classes/OnlyClass.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/classes/OnlyClass.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/classes/OnlyExtends.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/classes/OnlyExtends.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/classes/OnlyNew.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/classes/OnlyNew.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/dafny/DafnyIncrE1.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/dafny/DafnyIncrE1.java")
    configuration.verdict = Verdict.Fail
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

  it should "pass with silicon and examples/domains/list.sil" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/domains/list.sil")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/domains/option.sil" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/domains/option.sil")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/errors/constructor_requires_this.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/errors/constructor_requires_this.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/errors/not_a_location1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/errors/not_a_location1.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/errors/not_a_location2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/errors/not_a_location2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/errors/type_error_1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/errors/type_error_1.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/errors/type_error_2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/errors/type_error_2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/executable/src/separate/Main.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/executable/src/separate/Main.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/executable/src/separate/Util.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/executable/src/separate/Util.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/fixed-known-problems/FunctionProblemFixed1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/fixed-known-problems/FunctionProblemFixed1.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with silicon and examples/fixed-known-problems/FunctionProblemFixed2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/fixed-known-problems/FunctionProblemFixed2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/fixed-known-problems/rewriterIssue1.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/fixed-known-problems/rewriterIssue1.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/fixed-known-problems/rewriterIssue2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/fixed-known-problems/rewriterIssue2.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/fixed-known-problems/rewriterIssue3.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/fixed-known-problems/rewriterIssue3.pvl")
    configuration.verdict = Verdict.Fail
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


}
