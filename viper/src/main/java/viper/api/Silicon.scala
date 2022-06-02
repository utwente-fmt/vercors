package viper.api
import hre.config.Configuration
import org.slf4j.LoggerFactory.getLogger
import viper.silicon.logger.SymbExLogger
import viper.silver.plugin.{PluginAwareReporter, SilverPluginManager}
import viper.silver.reporter.Reporter
import viper.silver.verifier.Verifier

import java.nio.file.Path
import scala.annotation.nowarn

@nowarn("any") // due to be removed
case class Silicon(z3Settings: Map[String, String] = Map.empty, z3Path: Path = Resources.getZ3Path) extends SilverBackend {
  override def createVerifier(reporter: Reporter): (viper.silicon.Silicon, SilverPluginManager) = {
    val silicon = new viper.silicon.Silicon(reporter)

    val z3Config = '"' + z3Settings.map{case (k, v) => s"$k=$v"}.mkString(" ") + '"'

    var siliconConfig = Seq(
      "--z3Exe", z3Path.toString,
      "--z3ConfigArgs", z3Config,
      "--numberOfParallelVerifiers", "1",
      "--ideModeAdvanced",
    )

    if(Configuration.currentConfiguration.debugBackend.get()) {
      siliconConfig ++= Seq("--logLevel", "ALL")
    }

    siliconConfig :+= "-"

    silicon.parseCommandLine(siliconConfig)
    silicon.start()

    val plugins = SilverPluginManager(Some(Seq(
      "viper.silver.plugin.standard.termination.TerminationPlugin",
    ).mkString(":")))(silicon.reporter, getLogger("viper.silver.plugin").asInstanceOf[ch.qos.logback.classic.Logger], silicon.config)

    (silicon, plugins)
  }

  override def stopVerifier(verifier: Verifier): Unit = {
    verifier.stop()
    SymbExLogger.reset()
  }
}
