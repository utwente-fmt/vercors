package viper.api
import hre.config.Configuration
import hre.util.FileHelper
import viper.silver.reporter.Reporter
import viper.silver.verifier.Verifier

import java.nio.file.Path

case class Carbon(z3Path: Path = Resources.getZ3Path, boogiePath: Path = Resources.getBoogiePath, printFile: Option[Path] = None, proverLogFile: Option[Path] = None) extends SilverBackend {
  override def createVerifier(reporter: Reporter): viper.carbon.CarbonVerifier = {
    val reporter = EntityTrackingReporter()
    val carbon = viper.carbon.CarbonVerifier(reporter)

    var carbonConfig = Seq(
      "--z3Exe", z3Path.toString,
      "--boogieExe", boogiePath.toString,
    )

    printFile match {
      case Some(p) => carbonConfig ++= Seq("--print", p.toString)
      case _ =>
    }

    proverLogFile match {
      case Some(p) => carbonConfig ++= Seq("--proverLog", p.toString)
      case _ =>
    }

    carbonConfig ++= Seq("-")

    carbon.parseCommandLine(carbonConfig)

    carbon.start()
    carbon
  }

  override def stopVerifier(verifier: Verifier): Unit = {
    verifier.stop()
  }
}
