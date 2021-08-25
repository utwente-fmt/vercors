package integration

import hre.util.TestReport.Verdict
import integration.helper.{IntegrationTestConfiguration, IntegrationTestHelper}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

class FailingTests extends AnyFlatSpec with Matchers {
  //This test may take a long time to verify
  it should "fail with silicon and examples/openmp/sections-reduced-fail.c" taggedAs Slow in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/openmp/sections-reduced-fail.c")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    configuration.progress = true
    IntegrationTestHelper.test(configuration)
  }

}
