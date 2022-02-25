package viper.api

import hre.ast.OriginFactory
import hre.config.Configuration

import java.nio.file.Path
import java.util.Properties

class CarbonVerifier[O](o:OriginFactory[O]) extends SilverImplementation[O](o) {
  override def createVerifier(tool_home:Path,settings:Properties):viper.silver.verifier.Verifier = {
    val carbon = viper.carbon.CarbonVerifier(HREViperReporter(), Seq("startedBy" -> "example", "fullCmd" -> "dummy"))
    carbon.parseCommandLine(Seq(
        "--z3Exe", Configuration.getZ3Path.getAbsolutePath,
        "--boogieExe",Configuration.getBoogiePath.getAbsolutePath,
        "-"))
    carbon.start()
    carbon
  }

}

