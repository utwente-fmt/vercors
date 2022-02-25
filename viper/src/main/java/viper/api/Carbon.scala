package viper.api
import hre.config.Configuration
import viper.silver.verifier.Verifier

object Carbon extends SilverBackend {
  override def createVerifier: Verifier = {
    val carbon = viper.carbon.CarbonVerifier(HREViperReporter(), Seq("startedBy" -> "example", "fullCmd" -> "dummy"))
    carbon.parseCommandLine(Seq(
      "--z3Exe", Configuration.getZ3Path.getAbsolutePath,
      "--boogieExe", Configuration.getBoogiePath.getAbsolutePath,
      "-"
    ))
    carbon.start()
    carbon
  }

  override def stopVerifier(verifier: Verifier): Unit = {
    verifier.stop()
  }
}
