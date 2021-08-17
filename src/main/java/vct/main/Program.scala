package vct.main

import hre.lang.HREExitException
import hre.lang.System.{DebugException, Progress, Verdict, Warning}
import hre.tools.TimeKeeper
import hre.util.Notifier
import vct.main.options.{CommandLineOptions, OptionsParserTrait}
import vct.main.passes.PassesGeneratorTrait
import vct.test.CommandLineTesting

class Program(loggingSetup: LoggingSetupTrait, passesExecutioner: PassesExecutionerTrait, passesGenerator: PassesGeneratorTrait,
              fileParser: FileParserTrait, optionsParser: OptionsParserTrait) {

  def run(args: Array[String]): Int = {
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
        val report = fileParser.parseInputs(CommandLineOptions.inputPaths,timeKeeper)
        val passes = passesGenerator.getPasses(report)
        passesExecutioner.doPasses(passes,report,timeKeeper)
      }
    } catch {
      case e: HREExitException =>
        exit = e.exit
        if(exit != 0)
          Verdict("The final verdict is Error")
      case e: Throwable =>
        DebugException(e)
        Warning("An unexpected error occured in VerCors! "
          + "Please report an issue at https://github.com/utwente-fmt/vercors/issues/new. "
          + "You can see the full exception by adding '--debug vct.main.Main' to the flags.")
        Verdict("The final verdict is Error")
    } finally {
      Progress("entire run took %d ms", Long.box(System.currentTimeMillis - wallStart))
      if(CommandLineOptions.notifySetting.get()) {
        Notifier.notify("VerCors", "Verification is complete")
      }
    }
    exit
  }
}
