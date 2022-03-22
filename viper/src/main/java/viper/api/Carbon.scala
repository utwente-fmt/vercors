package viper.api
import hre.config.Configuration
import hre.util.FileHelper
import viper.silver.reporter.Reporter
import viper.silver.verifier.Verifier

import java.nio.file.Path

case class Carbon(z3Path: Path = Resources.getZ3Path, boogiePath: Path = Resources.getBoogiePath) extends SilverBackend {
  override def createVerifier(reporter: Reporter): viper.carbon.CarbonVerifier = {
    val reporter = EntityTrackingReporter()
    val carbon = viper.carbon.CarbonVerifier(reporter)

    carbon.parseCommandLine(Seq(
      "--z3Exe", z3Path.toString,
      "--boogieExe", boogiePath.toString,
      "-"
    ))

    carbon.start()
    carbon
  }

  override def stopVerifier(verifier: Verifier): Unit = {
    verifier.stop()
  }
}
