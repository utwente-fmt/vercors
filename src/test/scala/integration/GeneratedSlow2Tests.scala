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
}
