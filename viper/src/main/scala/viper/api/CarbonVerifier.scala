package viper.api

import java.nio.file.Path
import java.util.Properties

import hre.ast.OriginFactory
import hre.config.Configuration

class CarbonVerifier[O](o:OriginFactory[O]) extends SilverImplementation[O](o) {
  // TODO: Mono 6.8 seems to be bad. 6.0, 5.0 seem to work pretty well...? Can probably detect that
  override def createVerifier(tool_home:Path,settings:Properties):viper.silver.verifier.Verifier = {
    val carbon = viper.carbon.CarbonVerifier(Seq("startedBy" -> "example", "fullCmd" -> "dummy"))
    carbon.parseCommandLine(Seq(
        "--z3Exe", Configuration.getZ3Path.getAbsolutePath,
        "--boogieExe",Configuration.getBoogiePath.getAbsolutePath,
        "-"))
    carbon.start()
    carbon
  }

}

