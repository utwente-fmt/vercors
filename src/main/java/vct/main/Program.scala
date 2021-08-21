package vct.main

import hre.config.{Configuration, ConfigurationNonStatic}
import hre.lang.{HREExitException, ISystem}
import hre.lang.System.{DebugException, Progress, Verdict, Warning, setCurrentSystem}
import hre.tools.TimeKeeper
import hre.util.Notifier
import vct.main.options.CommandLineOptionsParserTrait
import vct.main.passes.PassesGeneratorTrait
import vct.test.CommandLineTesting

class Program(loggingSetup: LoggingSetupTrait, passesExecutioner: PassesExecutionerTrait, passesGenerator: PassesGeneratorTrait,
              fileParser: FileParserTrait, optionsParser: CommandLineOptionsParserTrait, system: ISystem, configuration: ConfigurationNonStatic) {

  def run(args: Array[String]): Int = {
    setCurrentSystem(system)
    //For the integration tests Program is called multiple times. This resets the configuration to the default.
    Configuration.currentConfiguration=configuration
    var exit = 0
    val wallStart = System.currentTimeMillis
    try {
      loggingSetup.setupLoggingBeforeOptions()
      optionsParser.parseOptions(args)
      loggingSetup.setupLogging()
      optionsParser.checkOptions()
      if (CommandLineTesting.enabled) CommandLineTesting.runTests()
      else {
        val timeKeeper: TimeKeeper = new TimeKeeper
        val report = fileParser.parseInputs(Configuration.currentConfiguration.inputPaths,timeKeeper)
        val passes = passesGenerator.getPasses(report)
        passesExecutioner.doPasses(passes,report,timeKeeper)
      }
    } catch {
      case e: HREExitException =>
        exit = e.exit
        if(exit != 0)
          Verdict("The final verdict is Error")
      case e: Throwable =>
        exit = 1
        DebugException(e)
        Warning("An unexpected error occured in VerCors! "
          + "Please report an issue at https://github.com/utwente-fmt/vercors/issues/new. "
          + "You can see the full exception by adding '--debug vct.main.Main' to the flags.")
        Verdict("The final verdict is Error")
    } finally {
      Progress("entire run took %d ms", Long.box(System.currentTimeMillis - wallStart))
      if(Configuration.currentConfiguration.notifySetting.get()) {
        Notifier.notify("VerCors", "Verification is complete")
      }
    }
    exit
  }
}
