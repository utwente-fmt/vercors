package viper.api
import hre.config.Configuration
import hre.util.FileHelper
import viper.silver.reporter.Reporter
import viper.silver.verifier.Verifier

object Carbon extends SilverBackend {
  override def createVerifier(reporter: Reporter): viper.carbon.CarbonVerifier = {
    val reporter = EntityTrackingReporter()
    val carbon = viper.carbon.CarbonVerifier(reporter)

    carbon.parseCommandLine(Seq(
      "--z3Exe", FileHelper.getZ3Path.getAbsolutePath,
      "--boogieExe", FileHelper.getBoogiePath.getAbsolutePath,
      "-"
    ))

    carbon.start()
    carbon
  }

  override def stopVerifier(verifier: Verifier): Unit = {
    verifier.stop()
  }
}
