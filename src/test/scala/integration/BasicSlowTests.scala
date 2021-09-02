package integration

import hre.util.Verdict
import integration.helper.{IntegrationTestConfiguration, IntegrationTestHelper}
import org.scalatest.Ignore
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow
import org.scalatest.time.{Millis, Span}



class BasicSlowTests extends AnyFlatSpec with TimeLimitedTests with Matchers {

  override def timeLimit: Span = Span(90000,Millis)

  it should "pass with silicon and examples/basic/AddAssignJava.java" taggedAs Slow in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/AddAssignJava.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/array-item-access.pvl" taggedAs Slow in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/array-item-access.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/pvl-array.pvl" taggedAs Slow in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/pvl-array.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/sumints.pvl" taggedAs Slow in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/sumints.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/BasicArray.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/BasicArray.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/InvokationInGuard.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/InvokationInGuard.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/seq-item-access.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/seq-item-access.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/test-1.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/test-1.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/basic/test-scale.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/test-scale.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/basic/BasicAssert-e1.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/BasicAssert-e1.java")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/BasicAssert.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/BasicAssert.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/BooleanOperators.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/BooleanOperators.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

}
