package viper.api

import java.nio.file.Path
import java.util.Properties

import hre.ast.OriginFactory
import viper.api.config.Configuration

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

class SiliconVerifier[O](o:OriginFactory[O]) extends SilverImplementation[O](o) {

  override def createVerifier(z3Path: Path, z3Settings: Properties):viper.silver.verifier.Verifier = {
    val silicon = new viper.silicon.Silicon(HREViperReporter(), Seq("startedBy" -> "example", "fullCmd" -> "dummy"))
    val z3Config = "\"" + z3Settings.asScala.map{case (key, vl) => s"$key=$vl"}.mkString(" ") + "\""

    val options = ArrayBuffer[String]()
    options ++= Seq("--z3Exe", z3Path.toString)
    options ++= Seq("--z3ConfigArgs", z3Config)

    if(Configuration.z3Progress.get) {
      options ++= Seq("--numberOfParallelVerifiers", "1")
    }

    options += "-"

    silicon.parseCommandLine(options)
	  silicon.start()
    silicon
  }

}
