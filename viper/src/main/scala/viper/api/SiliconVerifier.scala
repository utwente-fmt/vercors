package viper.api

import java.nio.file.Path
import java.util.Properties

import hre.ast.OriginFactory
import hre.config.Configuration
import viper.silver.plugin.PluginAwareReporter

import scala.jdk.CollectionConverters._

class SiliconVerifier[O](o:OriginFactory[O]) extends SilverImplementation[O](o) {
  override def createVerifier(z3Path: Path, z3Settings: Properties):viper.silver.verifier.Verifier = {
    val silicon = new viper.silicon.Silicon(PluginAwareReporter(HREViperReporter()), Seq("startedBy" -> "example", "fullCmd" -> "dummy"))

//    val z3Config = s""" "${z3Settings.asScala.map{case (k, v) => s"$k=$v"}.mkString(" ")}" """
    val z3Config = ""

    var siliconConfig = Seq(
      "--z3Exe", z3Path.toString,
      "--z3ConfigArgs", z3Config,
    )

    if(Configuration.debugBackend.get()) {
      siliconConfig ++= Seq("--logLevel", "ALL")
    }

    siliconConfig :+= "-"

    silicon.parseCommandLine(siliconConfig)
    silicon.start()
    silicon
  }
}
