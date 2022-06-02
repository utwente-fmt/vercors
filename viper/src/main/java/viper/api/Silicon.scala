package viper.api
import hre.config.Configuration
import viper.silicon.logger.SymbExLogger
import viper.silver.plugin.PluginAwareReporter
import viper.silver.reporter.Reporter
import viper.silver.verifier.Verifier

import java.nio.file.Path
import scala.annotation.nowarn

@nowarn("any") // due to be removed
case class Silicon(z3Settings: Map[String, String] = Map.empty, z3Path: Path = Resources.getZ3Path, numberOfParallelVerifiers: Option[Int] = None, logLevel: Option[String] = None) extends SilverBackend {
  override def createVerifier(reporter: Reporter): viper.silicon.Silicon = {
    val silicon = new viper.silicon.Silicon(reporter)

    val z3Config = '"' + z3Settings.map{case (k, v) => s"$k=$v"}.mkString(" ") + '"'

    var siliconConfig = Seq(
      "--z3Exe", z3Path.toString,
      "--z3ConfigArgs", z3Config,
    )

    if(Configuration.currentConfiguration.debugBackend.get()) {
      siliconConfig ++= Seq("--logLevel", "ALL")
    } else logLevel match {
      case Some(level) =>
        siliconConfig ++= Seq("--logLevel", level)
      case _ =>
    }

    numberOfParallelVerifiers match {
      case Some(n) =>
        siliconConfig ++= Seq("--numberOfParallelVerifiers", n + "")
      case None =>
    }

    siliconConfig :+= "-"

    silicon.parseCommandLine(siliconConfig)
    silicon.start()
    silicon
  }

  override def stopVerifier(verifier: Verifier): Unit = {
    verifier.stop()
    SymbExLogger.reset()
  }
}
