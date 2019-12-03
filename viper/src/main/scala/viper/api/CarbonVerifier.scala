package viper.api

import java.nio.file.Path
import java.util.Properties

class CarbonVerifier[O,Err](o:OriginFactory[O]) extends SilverImplementation[O,Err](o) {
  override def createVerifier(tool_home:Path,settings:Properties):viper.silver.verifier.Verifier = {
    def OS=System.getProperty("os.name");
    val z3_exe=if(OS.startsWith("Windows")){
      tool_home.resolve("z3").resolve("4.3.0").resolve("Windows NT").resolve("intel").resolve("bin").resolve("z3.exe").toString()
    } else if(OS.startsWith("Mac")){
      tool_home.resolve("z3").resolve("4.3.1").resolve("Darwin").resolve("x86_64").resolve("bin").resolve("z3").toString()
    } else {
      tool_home.resolve("z3").resolve("4.3.1").resolve("Linux").resolve("x86_64").resolve("bin").resolve("z3").toString()
    }
    val boogie_exe=if(OS.startsWith("Windows")){
      tool_home.resolve("boogie").resolve("2012-10-22").resolve("windows").resolve("bin").resolve("boogie").toString()
    } else {
      tool_home.resolve("boogie").resolve("2012-10-22").resolve("unix").resolve("bin").resolve("boogie").toString()
    }
    val carbon = new viper.carbon.CarbonVerifier(Seq("startedBy" -> "example", "fullCmd" -> "dummy"))
    print(Seq(
        "--z3Exe",z3_exe,
        "--boogieExe",boogie_exe,
        "-"));
    carbon.parseCommandLine(Seq(
        "--z3Exe",z3_exe,
        "--boogieExe",boogie_exe,
        "-"))
    //carbon.config.initialize{case _ =>}
    carbon.start()
    carbon
  }
}

