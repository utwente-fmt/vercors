package integration

import hre.util.TestReport.Verdict
import integration.helper.{IntegrationTestConfiguration, IntegrationTestHelper}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FailingTests extends AnyFlatSpec with Matchers {

  "VerCors" should "pass with array-item-access.pvl after MultiDimArray.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/MultiDimArray.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)

    val configuration2 = IntegrationTestConfiguration()
    configuration2.files = Array("examples/basic/array-item-access.pvl")
    configuration2.verdict = Verdict.Pass
    configuration2.toolSilicon = true
    IntegrationTestHelper.test(configuration2)
  }

  it should "pass with array-item-access.pvl" in {
    val configuration2 = IntegrationTestConfiguration()
    configuration2.files = Array("examples/basic/array-item-access.pvl")
    configuration2.verdict = Verdict.Pass
    configuration2.toolSilicon = true
    IntegrationTestHelper.test(configuration2)
  }

  it should "pass with silicon and examples/openmp/sections-reduced.c" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/openmp/sections-reduced.c")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainConstructor/MainConstructorBlock.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainConstructor/MainConstructorBlock.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

}
