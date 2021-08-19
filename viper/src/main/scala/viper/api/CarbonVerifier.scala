package viper.api

import java.nio.file.Path
import java.util.Properties
import hre.ast.OriginFactory
import hre.config.Configuration
import hre.util.FileHelper

class CarbonVerifier[O](o:OriginFactory[O]) extends SilverImplementation[O](o) {
  override def createVerifier(tool_home:Path,settings:Properties):viper.silver.verifier.Verifier = {
    val carbon = viper.carbon.CarbonVerifier(Seq("startedBy" -> "example", "fullCmd" -> "dummy"))
    carbon.parseCommandLine(Seq(
        "--z3Exe", FileHelper.getZ3Path.getAbsolutePath,
        "--boogieExe",FileHelper.getBoogiePath.getAbsolutePath,
        "-"))
    carbon.start()
    carbon
  }

}

