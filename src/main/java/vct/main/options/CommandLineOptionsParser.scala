package vct.main.options

import hre.config.{Configuration, OptionParser}
import hre.lang.HREExitException
import hre.lang.System.{Fail, Output}
import vct.main.BuildInfo
import vct.main.passes.Passes
import vct.test.CommandLineTesting

import java.time.Instant
import scala.jdk.CollectionConverters.IterableHasAsScala

trait OptionsParserTrait{
  def parseOptions(args: Array[String]): Unit
  def checkOptions(): Unit
}
class OptionsParser extends OptionsParserTrait{

  def parseOptions(args: Array[String]): Unit = {
    val parser = new OptionParser()
    parser.add(parser.getHelpOption, Char.box('h'), "help")
    CommandLineOptions.addOptions(parser)
    CommandLineTesting.addOptions(parser)
    Configuration.addOptions(parser)
    val files = parser.parse(args) //These are the arguments which did not match anything. They should be the files.
    if (Configuration.veymont_file.get() != null) {
      files ++ Configuration.getVeyMontFiles.map(_.getAbsolutePath())
    }
    CommandLineOptions.inputPaths = files
  }

  def checkOptions(): Unit = {
    if (CommandLineOptions.version.get) {
      Output("%s %s", BuildInfo.name, BuildInfo.version)
      Output("Built by sbt %s, scala %s at %s", BuildInfo.sbtVersion, BuildInfo.scalaVersion, Instant.ofEpochMilli(BuildInfo.builtAtMillis))
      if (BuildInfo.currentBranch != "master")
        Output("On branch %s, commit %s, %s",
          BuildInfo.currentBranch, BuildInfo.currentShortCommit, BuildInfo.gitHasChanges)

      throw new HREExitException(0)
    }

    if (CommandLineOptions.helpPasses.get) {
      Output("The following passes are available:")
      Passes.BY_KEY.foreach {
        case (key, pass) => Output(" %-12s : %s", key, pass.description)
      }
      throw new HREExitException(0)
    }

    if(Seq(
      CommandLineTesting.enabled,
      CommandLineOptions.silver.used,
      CommandLineOptions.passList.asScala.nonEmpty,
      Configuration.veymont_file.used()
    ).forall(!_)) {
      Fail("no back-end or passes specified")
    }

    if (CommandLineOptions.stopBeforeBackend.get() && CommandLineOptions.stopAfterTypecheck.get()) {
      Fail("The --stop-before-backend and --stop-after-typecheck flags are mutually exclusive.")
    }

    if (CommandLineOptions.silver.used) CommandLineOptions.silver.get match {
      case "silicon" => // Nothing to check for
      case "carbon" => // Nothing to check for
      case _ =>
        Fail("unknown silver backend: %s", CommandLineOptions.silver.get)
    }

    val vFile = Configuration.veymont_file.get()
    if(vFile != null) {
      val nonPVL = CommandLineOptions.inputPaths.filter(!_.endsWith(".pvl"))
      if(nonPVL.nonEmpty)
        Fail("VeyMont cannot use non-PVL files %s",nonPVL.mkString(", "))
      if(!vFile.endsWith(".pvl"))
        Fail("VeyMont cannot output to non-PVL file %s",vFile)
    }
  }
}
