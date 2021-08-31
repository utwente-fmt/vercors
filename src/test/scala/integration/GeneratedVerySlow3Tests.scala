package integration

import hre.util.Verdict
import integration.helper.{IntegrationTestConfiguration, IntegrationTestHelper}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


/*+
  These tests have been generated based on the old test framework. For new test I would recommend splitting it up in
  different classes and giving more descriptive names.
 */

class GeneratedVerySlow3Tests extends AnyFlatSpec with Matchers {


  it should "pass with silicon and examples/demo/demo3a-func-with-lemmas.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/demo/demo3a-func-with-lemmas.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "fail with silicon and examples/demo/demo3a-func.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/demo/demo3a-func.pvl")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/demo/demo3d.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/demo/demo3d.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/demo/demo4.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/demo/demo4.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/floats/TestHist.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/floats/TestHist.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/futures/NoSendAfterRead.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/futures/NoSendAfterRead.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/futures/TestFuture.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/futures/TestFuture.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/futures/TestFuturePermsPass.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/futures/TestFuturePermsPass.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/futures/counteradd_2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/futures/counteradd_2.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/futures/counteradd_n.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/futures/counteradd_n.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/futures/unequalcounting.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/futures/unequalcounting.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    configuration.checkHistory = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/graphs/reach.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/graphs/reach.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }
}
