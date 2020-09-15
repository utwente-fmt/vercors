package viper.api

import java.nio.file.Path
import java.util.Properties

import hre.ast.OriginFactory
import hre.config.Configuration

class CarbonVerifier[O](o:OriginFactory[O]) extends SilverImplementation[O](o) {
  // TODO: Detect if mono is present
  // TODO: Give boogie.exe if on windows
  override def createVerifier(tool_home:Path,settings:Properties):viper.silver.verifier.Verifier = {
    val carbon = viper.carbon.CarbonVerifier(Seq("startedBy" -> "example", "fullCmd" -> "dummy"))
    hre.lang.System.Output(Seq(
        "--z3Exe", Configuration.getZ3Path.getAbsolutePath,
        "--boogieExe", Configuration.getBoogiePath.getAbsolutePath,
        "-").mkString(" "))
    carbon.parseCommandLine(Seq(
        "--z3Exe", Configuration.getZ3Path.getAbsolutePath,
        "--boogieExe",Configuration.getBoogiePath.getAbsolutePath,
        "-"))
    //carbon.config.initialize{case _ =>}
    carbon.start()
    carbon
  }
}

