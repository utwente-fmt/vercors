package integration.helper

import ch.qos.logback.classic.{Level, Logger}
import hre.config.ConfigurationNonStatic
import hre.lang.{ISystem, LogLevel}
import hre.util.Verdict
import org.scalatest.Assertions.{assert, assertResult, fail}
import org.scalatest.Checkpoints.Checkpoint
import org.scalatest.exceptions.TestFailedException
import org.slf4j.LoggerFactory
import vct.main.{FileParser, PassesExecutioner, Program, Vercors}
import vct.main.options.CommandLineOptionsParser
import vct.main.passes.PassesGenerator
import vct.options.{Backend, Mode, Options}
import vct.result.VerificationResult

import java.nio.file.Paths
import java.util
import scala.jdk.javaapi.CollectionConverters.asJavaCollection

object IntegrationTestHelper {
  /*
  def test(configuration: IntegrationTestConfiguration): Unit ={
    val checkPoint = new Checkpoint
    val system = createSystem(configuration,checkPoint)
    val streamListener = createStreamListener(system)
    val arguments = createArguments(configuration)
    val program = createProgram(system)

    val exitCode = program.run(arguments)

    checkEndConditions(configuration,exitCode,system,checkPoint,streamListener)
  }
  */

  def test(configuration: IntegrationTestConfiguration): Unit = {
    LoggerFactory.getLogger("viper").asInstanceOf[Logger].setLevel(Level.OFF)

    val options = Options(
      inputs = configuration.files.map(Paths.get(_)),
      mode = if(configuration.toolVeymont) Mode.VeyMont else Mode.Verify,
      skipBackend = configuration.stopBeforeBackend,
    )

    if(configuration.toolSilicon) checkResult(options.copy(backend = Backend.Silicon), configuration.verdict)
    if(configuration.toolCarbon) checkResult(options.copy(backend = Backend.Carbon), configuration.verdict)
  }

  def checkResult(options: Options, verdict: Verdict): Unit = {
    Vercors(options).go() match {
      case error: VerificationResult.UserError =>
        assert(verdict == Verdict.Error)
      case error: VerificationResult.SystemError =>
        fail(error)
      case VerificationResult.Ok =>
        assert(verdict == Verdict.Pass || verdict == Verdict.Fail)
    }
  }

  def createSystem(configuration: IntegrationTestConfiguration,cp: Checkpoint): SystemListener ={
    var message = ""
    configuration.verdict match {
      case Verdict.Error => message = "The final verdict is Error"
      case Verdict.Pass => message = "The final verdict is Pass"
      case Verdict.Inconclusive => cp(() => {fail("Verdict inconclusive is not supported")})
      case Verdict.Fail => message = "The final verdict is Fail"
    }
    new SystemListener(message)
  }

  private def createStreamListener(system: ISystem) = {
    val streamListener = new StreamListener
    system.setErrorStream(streamListener, LogLevel.Debug)
    system.setOutputStream(streamListener, LogLevel.Debug)
    streamListener
  }

  def createArguments(configuration: IntegrationTestConfiguration): Array[String] ={
    val arguments = new util.ArrayList[String]

    if(configuration.progress){
      arguments.add("--progress")
    }
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

  def createProgram(system: ISystem): Program ={
    val loggingSetup = new TestLoggingSetup
    val passesExecutioner = new PassesExecutioner
    val passesGenerator = new PassesGenerator
    val fileParser = new FileParser
    val optionsParser = new CommandLineOptionsParser
    val configuration = new ConfigurationNonStatic
    new Program(loggingSetup,passesExecutioner,passesGenerator,fileParser,optionsParser,system,configuration)
  }

  def checkEndConditions(configuration: IntegrationTestConfiguration, exitCode: Int, systemListener: SystemListener,
                         cp: Checkpoint,streamListener: StreamListener): Unit ={
    cp.apply({assert(systemListener.getFoundExpectedMessage(),"Did not find verdict message \""+ systemListener.getExpectedVerdictMessage + "\" in output.")})
    configuration.verdict match {
      case Verdict.Error => cp.apply(() => {assertResult(1,"For verdict error exitcode should be 1"){exitCode}})
      case Verdict.Pass => cp.apply(() => {assertResult(0,"For verdict pass exitcode should be 0"){exitCode}})
      case Verdict.Inconclusive => cp.apply(() => {fail("Verdict inconclusive is not supported")})
      case Verdict.Fail => cp.apply(() => {assertResult(0,"For verdict fail exitcode should be 0"){exitCode}})
    }

    try {
      cp.reportAll()
    } catch {
      case _: TestFailedException =>
        var logMessage =  "\nLog generated by VerCors\n"
        logMessage +=  streamListener.log
        logMessage += "\n End of log.\n"
        cp.apply({assert(false,logMessage)})
        cp.reportAll()
    }
  }

}
