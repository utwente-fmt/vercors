package viper.api
import hre.config.Configuration
import hre.util.FileHelper
import viper.silver.verifier.Verifier

object Carbon extends SilverBackend {
  override def createVerifier: (Verifier, EntityTrackingReporter) = {
    val reporter = EntityTrackingReporter()
    val carbon = viper.carbon.CarbonVerifier(reporter)

    carbon.parseCommandLine(Seq(
      "--z3Exe", FileHelper.getZ3Path.getAbsolutePath,
      "--boogieExe", FileHelper.getBoogiePath.getAbsolutePath,
      "-"
    ))

    carbon.start()
    (carbon, reporter)
  }

  override def stopVerifier(verifier: Verifier): Unit = {
    verifier.stop()
  }
}
