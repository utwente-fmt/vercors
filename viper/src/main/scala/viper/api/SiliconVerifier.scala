package viper.api

import java.nio.file.Path
import java.util.Properties

import hre.ast.OriginFactory
import hre.config.Configuration
import viper.silver.plugin.PluginAwareReporter

import scala.jdk.CollectionConverters._

class SiliconVerifier[O](o:OriginFactory[O]) extends SilverImplementation[O](o) {
  override def createVerifier(z3Path: Path, z3Settings: Properties, backendOptions: Seq[String]):viper.silver.verifier.Verifier = {
    val silicon = new viper.silicon.Silicon(PluginAwareReporter(HREViperReporter()), Seq("startedBy" -> "example", "fullCmd" -> "dummy"))

    val z3KeyValues = z3Settings.asScala.map{case (k, v) => s"$k=$v"}.mkString("\"", " ", "\"")

    var siliconConfig = Seq(
      "--z3Exe", z3Path.toString,
      "--z3ConfigArgs", z3KeyValues,
      "--numberOfParallelVerifiers", Configuration.silicon_num_verifiers.get().toString,
    )

    if(Configuration.debugBackend.get()) {
      siliconConfig ++= Seq("--logLevel", "ALL")
    }

    siliconConfig ++= backendOptions

    siliconConfig :+= "-"

    silicon.parseCommandLine(siliconConfig)
    silicon.start()
    silicon
  }
}
