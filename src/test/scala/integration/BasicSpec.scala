package integration

import hre.util.TestReport.Verdict
import integration.helper.{IntegrationTestConfiguration, IntegrationTestHelper}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class BasicSpec extends AnyFlatSpec with Matchers {

  //todo
  //DONE split main
  //DONE stderr out should be allowed for testing
  //listen in system
  //DONE make generic class for testing
  //DONE make integrationTestConfiguration
  //NAH create one place for all the settings (Maybe split it in multiple objects)
  //Generate tests automatically
  //Fix Veymont
  //create something for Veymont

  val folder = "examples/basic/"

  "VerCors" should folder + "AddAssignJava.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.file = folder + "AddAssignJava.java"
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should folder+"array-item-access.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.file = folder+"array-item-access.pvl"
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should folder+"ArrayAsObject.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.file = folder+"ArrayAsObject.java"
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should folder+"BasicArray.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.file = folder+"BasicArray.java"
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should folder+"BasicAssert.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.file = folder+"BasicAssert.java"
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should folder+"BasicAssert-e1.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.file = folder+"BasicAssert-e1.java"
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should folder+"BooleanOperators.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.file = folder+"BooleanOperators.java"
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should folder+"Boxing.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.file = folder+"Boxing.java"
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should folder+"bracket-issues.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.file = folder+"bracket-issues.pvl"
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }
  //This one did not have a verdict?
  it should folder+"CollectionTest.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.file = folder+"CollectionTest.pvl"
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

  it should folder+"constructor-name.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.file = folder+"constructor-name.pvl"
    configuration.verdict = Verdict.Error
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }
}
