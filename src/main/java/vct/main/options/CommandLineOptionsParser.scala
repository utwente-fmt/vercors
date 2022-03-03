package vct.main.options

import hre.config.{Configuration, OptionParser}
import hre.lang.HREExitException
import hre.lang.System.{Fail, Output}
import hre.util.FileHelper
import vct.main.BuildInfo
import vct.main.passes.Passes
import vct.test.CommandLineTesting

import java.time.Instant
import scala.jdk.CollectionConverters.IterableHasAsScala

trait CommandLineOptionsParserTrait{
  def parseOptions(args: Array[String]): Unit
  def checkOptions(): Unit
}
class CommandLineOptionsParser extends CommandLineOptionsParserTrait{

  def parseOptions(args: Array[String]): Unit = {
    val parser = new OptionParser()
    parser.add(parser.getHelpOption, Char.box('h'), "help")
    Configuration.currentConfiguration.addOptions(parser)
    CommandLineTesting.addOptions(parser)
    var files = parser.parse(args) //These are the arguments which did not match anything. They should be the files.
    if (Configuration.currentConfiguration.veymont_file.get() != null) {
      val veymontFiles = FileHelper.getVeyMontFiles.map(_.getAbsolutePath())
      files = files ++ veymontFiles
    }
    Configuration.currentConfiguration.inputPaths = files
  }

  def checkOptions(): Unit = {
    if (Configuration.currentConfiguration.version.get) {
      Output("%s %s", BuildInfo.name, BuildInfo.version)
      Output("Built by sbt %s, scala %s at %s", BuildInfo.sbtVersion, BuildInfo.scalaVersion, Instant.ofEpochMilli(BuildInfo.builtAtMillis))
      if (BuildInfo.currentBranch != "master")
        Output("On branch %s, commit %s, %s",
          BuildInfo.currentBranch, BuildInfo.currentShortCommit, BuildInfo.gitHasChanges)

      throw new HREExitException(0)
    }

    if (Configuration.currentConfiguration.helpPasses.get) {
      Output("The following passes are available:")
      Passes.BY_KEY.foreach {
        case (key, pass) => Output(" %-12s : %s", key, pass.description)
      }
      throw new HREExitException(0)
    }

    if(Seq(
      CommandLineTesting.enabled,
      Configuration.currentConfiguration.silver.used,
      Configuration.currentConfiguration.passList.asScala.nonEmpty,
      Configuration.currentConfiguration.veymont_file.used()
    ).forall(!_)) {
      Fail("no back-end or passes specified")
    }

    if (Configuration.currentConfiguration.stopBeforeBackend.get() && Configuration.currentConfiguration.stopAfterTypecheck.get()) {
      Fail("The --stop-before-backend and --stop-after-typecheck flags are mutually exclusive.")
    }

    if (Configuration.currentConfiguration.silver.used) Configuration.currentConfiguration.silver.get match {
      case "silicon" => // Nothing to check for
      case "carbon" => // Nothing to check for
      case _ =>
        Fail("unknown silver backend: %s", Configuration.currentConfiguration.silver.get)
    }

    val vFile = Configuration.currentConfiguration.veymont_file.get()
    if(vFile != null) {
      val nonPVL = Configuration.currentConfiguration.inputPaths.filter(!_.endsWith(".pvl"))
      if(nonPVL.nonEmpty)
        Fail("VeyMont cannot use non-PVL files %s",nonPVL.mkString(", "))
      if(!vFile.endsWith(".pvl"))
        Fail("VeyMont cannot output to non-PVL file %s",vFile)
    }
  }
}
