package viper.api

import java.nio.file.Path
import java.util.Properties

import hre.ast.OriginFactory
import hre.config.Configuration

class CarbonVerifier[O](o:OriginFactory[O]) extends SilverImplementation[O](o) {
  override def createVerifier(tool_home:Path,settings:Properties,backendOptions: Seq[String]):viper.silver.verifier.Verifier = {
    val carbon = viper.carbon.CarbonVerifier(new HREViperReporter, Seq("startedBy" -> "example", "fullCmd" -> "dummy"))
    carbon.parseCommandLine(Seq(
        "--z3Exe", Configuration.getZ3Path.getAbsolutePath,
        "--boogieExe",Configuration.getBoogiePath.getAbsolutePath) ++ backendOptions ++
        Seq("-"))
    carbon.start()
    carbon
  }

}

