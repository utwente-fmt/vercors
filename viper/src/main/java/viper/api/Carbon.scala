package viper.api
import hre.config.Configuration
import viper.silver.verifier.Verifier

object Carbon extends SilverBackend {
  override def createVerifier: (Verifier, EntityTrackingReporter) = {
    val reporter = EntityTrackingReporter()
    val carbon = viper.carbon.CarbonVerifier(reporter)

    carbon.parseCommandLine(Seq(
      "--z3Exe", Configuration.getZ3Path.getAbsolutePath,
      "--boogieExe", Configuration.getBoogiePath.getAbsolutePath,
      "-"
    ))

    carbon.start()
    (carbon, reporter)
  }

  override def stopVerifier(verifier: Verifier): Unit = {
    verifier.stop()
  }
}
