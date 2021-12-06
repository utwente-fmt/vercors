package viper.api
import hre.config.Configuration
import viper.silver.plugin.PluginAwareReporter
import viper.silver.verifier.Verifier

import java.nio.file.Path
import scala.annotation.nowarn

@nowarn("any") // due to be removed
case class Silicon(z3Settings: Map[String, String], z3Path: Path) extends SilverBackend {
  override def createVerifier: Verifier = {
    val silicon = new viper.silicon.Silicon(PluginAwareReporter(HREViperReporter()), Seq("startedBy" -> "example", "fullCmd" -> "dummy"))

    val z3Config = '"' + z3Settings.map{case (k, v) => s"$k=$v"}.mkString(" ") + '"'

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
