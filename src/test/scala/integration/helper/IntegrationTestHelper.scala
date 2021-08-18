package integration.helper

import hre.util.TestReport.Verdict
import vct.main.{FileParser, PassesExecutioner, Program}
import vct.main.options.OptionsParser
import vct.main.passes.PassesGenerator

import java.util
import scala.jdk.javaapi.CollectionConverters.asJavaCollection

object IntegrationTestHelper {

  def test(configuration: IntegrationTestConfiguration): Unit ={
    val arguments = createArguments(configuration)
    val program = createProgram()
    val exitCode = program.run(arguments)
    checkExitCode(configuration,exitCode)
  }

  def createArguments(configuration: IntegrationTestConfiguration): Array[String] ={
    val arguments = new util.ArrayList[String]
    arguments.add("--progress")
    arguments.add("--check-history")

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

  def createProgram(): Program ={
    val loggingSetup = new TestLoggingSetup
    val passesExecutioner = new PassesExecutioner
    val passesGenerator = new PassesGenerator
    val fileParser = new FileParser
    val optionsParser = new OptionsParser
    new Program(loggingSetup,passesExecutioner,passesGenerator,fileParser,optionsParser)
  }

  def checkExitCode(configuration: IntegrationTestConfiguration,exitCode: Int): Unit ={
    configuration.verdict match {
      case Verdict.Error => assert(exitCode==1)
      case Verdict.Pass => assert(exitCode==0)
      case Verdict.Inconclusive => assert(exitCode==1)
      case Verdict.Fail => assert(exitCode==0)
    }
  }

}
