package integration.helper

import hre.config.{Configuration, ConfigurationNonStatic}
import hre.lang.ISystem
import hre.util.TestReport.Verdict
import org.scalatest.Assertions.{assertResult, fail}
import vct.main.{FileParser, PassesExecutioner, Program}
import vct.main.options.OptionsParser
import vct.main.passes.PassesGenerator

import java.util
import scala.jdk.javaapi.CollectionConverters.asJavaCollection

object IntegrationTestHelper {

  def test(configuration: IntegrationTestConfiguration): Unit ={
    resetStaticConfiguration()
    val arguments = createArguments(configuration)
    val system = createSystem(configuration)
    val program = createProgram(system)

    val exitCode = program.run(arguments)

    checkEndConditions(configuration,exitCode,system)
  }

  def createArguments(configuration: IntegrationTestConfiguration): Array[String] ={
    val arguments = new util.ArrayList[String]
    //arguments.add("--progress")
    //arguments.add("--debug vct.main.Program")

    if(configuration.toolSilicon){
      arguments.add("--silicon")
    }
    if(configuration.toolCarbon){
      arguments.add("--carbon")
    }
    if(configuration.toolVeymont) {
      arguments.add("--veymont")
      arguments.add("tmp.pvl")
    }else{
      // VeyMont was not built with the feature system in mind, so we have to disable it when veymont is executed
      // in the test suite.
      arguments.add("--strict-internal")
    }
    if(configuration.disableSat){
      arguments.add("--disable-sat")
    }
    if(configuration.checkHistory){
      arguments.add("--check-history")
    }
    if(configuration.checkDefined){
      arguments.add("--check-defined")
    }
    if(configuration.checkAxioms){
      arguments.add("--check-axioms")
    }
    if(configuration.stopBeforeBackend){
      arguments.add("--stop-before-backend")
    }

    arguments.addAll(asJavaCollection(configuration.files))

    val argumentArray = new Array[String](arguments.size)
    arguments.toArray(argumentArray)
    argumentArray
  }

  def createSystem(configuration: IntegrationTestConfiguration): SystemListener ={
    var message = ""
    configuration.verdict match {
      case Verdict.Error => message = "The final verdict is Error"
      case Verdict.Pass => message = "The final verdict is Pass"
      case Verdict.Inconclusive => fail("Verdict inconclusive is not supported")
      case Verdict.Fail => message = "The final verdict is Fail"
    }
    new SystemListener(message)
  }

  def createProgram(system: ISystem): Program ={
    val loggingSetup = new TestLoggingSetup
    val passesExecutioner = new PassesExecutioner
    val passesGenerator = new PassesGenerator
    val fileParser = new FileParser
    val optionsParser = new OptionsParser
    val configuration = new ConfigurationNonStatic
    new Program(loggingSetup,passesExecutioner,passesGenerator,fileParser,optionsParser,system,configuration)
  }

  def checkEndConditions(configuration: IntegrationTestConfiguration, exitCode: Int, systemListener: SystemListener): Unit ={
    assert(systemListener.getFoundExpectedMessage(),"Did not find verdict message "+ systemListener.getExpectedVerdictMessage + " in output.")
    configuration.verdict match {
      case Verdict.Error => assertResult(1,"For verdict error exitcode should be 1"){exitCode}
      case Verdict.Pass => assertResult(0,"For verdict pass exitcode should be 0"){exitCode}
      case Verdict.Inconclusive => fail("Verdict inconclusive is not supported")
      case Verdict.Fail => assertResult(0,"For verdict fail exitcode should be 0"){exitCode}
    }
  }

  def resetStaticConfiguration(): Unit ={

  }

}
