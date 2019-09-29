package viper.api

import java.nio.file.Path
import java.util.Properties
import scala.collection.JavaConversions._

class SiliconVerifier[O,Err](o:OriginFactory[O]) extends SilverImplementation[O,Err](o) {

  override def createVerifier(tool_home: Path,props: Properties):viper.silver.verifier.Verifier = {
    def OS=System.getProperty("os.name");
//    println("tool home is "+tool_home);
    /*
        When updating z3, please check whether the custom options set in vct.silver.SilverBackend still apply.
     */
    val z3_exe=if(OS.startsWith("Windows")){
      tool_home.resolve("z3").resolve("4.8.5").resolve("Windows NT").resolve("intel").resolve("bin").resolve("z3.exe").toString()
    } else if(OS.startsWith("Mac")){
      tool_home.resolve("z3").resolve("4.8.5").resolve("Darwin").resolve("x86_64").resolve("bin").resolve("z3").toString()
    } else {
      tool_home.resolve("z3").resolve("4.8.5").resolve("Linux").resolve("x86_64").resolve("bin").resolve("z3").toString()
    }
    val silicon = new viper.silicon.Silicon(HREViperReporter(), Seq("startedBy" -> "example", "fullCmd" -> "dummy"))
    var z3_config="\"";
    var sep="";
    props.foreach {
      entry => z3_config=z3_config+sep+(entry._1)+"="+(entry._2) ; sep=" "
    }
    z3_config+="\"";
    //println(z3_config);
    silicon.parseCommandLine(Seq(
        "--z3Exe",z3_exe,
        "--z3ConfigArgs",z3_config,
        "-"))
				
	  /*
    silicon.config.initialize {
    	case _ => silicon.config.initialized = true
    }
		*/
		
    silicon.start()
    silicon
  }

}
