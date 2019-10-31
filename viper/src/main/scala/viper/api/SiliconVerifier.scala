package viper.api

import java.nio.file.Path
import java.util.Properties
import scala.collection.JavaConversions._

class SiliconVerifier[O,Err](o:OriginFactory[O]) extends SilverImplementation[O,Err](o) {

  override def createVerifier(z3Path: Path, z3Settings: Properties):viper.silver.verifier.Verifier = {
    val silicon = new viper.silicon.Silicon(HREViperReporter(), Seq("startedBy" -> "example", "fullCmd" -> "dummy"))
    var z3_config="\"";
    var sep="";
    z3Settings.foreach {
      entry => z3_config=z3_config+sep+(entry._1)+"="+(entry._2) ; sep=" "
    }
    z3_config+="\"";
    //println(z3_config);
    silicon.parseCommandLine(Seq(
        "--z3Exe", z3Path.toString(),
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
