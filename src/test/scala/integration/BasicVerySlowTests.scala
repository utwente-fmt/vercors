package integration

import hre.util.Verdict
import integration.helper.{IntegrationTestConfiguration, IntegrationTestHelper}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

class BasicVerySlowTests extends AnyFlatSpec with Matchers {

  it should "pass with silicon and examples/basic/MultiDimArray.java" taggedAs Slow in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/MultiDimArray.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/pointer.c" taggedAs Slow in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/pointer.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with silicon and examples/basic/satcheck-check.pvl" taggedAs Slow in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/satcheck-check.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

}
