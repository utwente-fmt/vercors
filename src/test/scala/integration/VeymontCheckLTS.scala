package integration

import hre.util.TestReport.Verdict
import integration.helper.{IntegrationTestConfiguration, IntegrationTestHelper}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VeymontCheckLTS extends AnyFlatSpec with Matchers {

  val folder = "examples/veymont-check/checkLTS/"

  "VerCors" should folder + "simplemethodcall.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.file = folder + "simplemethodcall.pvl"
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }
}
