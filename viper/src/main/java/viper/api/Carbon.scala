package viper.api
import hre.config.Configuration
import hre.util.FileHelper
import org.slf4j.LoggerFactory.getLogger
import viper.silver.plugin.SilverPluginManager
import viper.silver.reporter.Reporter
import viper.silver.verifier.Verifier

import java.nio.file.Path

case class Carbon(z3Path: Path = Resources.getZ3Path, boogiePath: Path = Resources.getBoogiePath, printFile: Option[Path] = None, proverLogFile: Option[Path] = None, options: Seq[String] = Nil) extends SilverBackend {
  override def createVerifier(reporter: Reporter): (viper.carbon.CarbonVerifier, SilverPluginManager) = {
    val reporter = EntityTrackingReporter()
    val carbon = viper.carbon.CarbonVerifier(reporter)

    val carbonConfig = Seq(
      "--z3Exe", z3Path.toString,
      "--boogieExe", boogiePath.toString,
    ) ++ (printFile match {
      case Some(p) => Seq("--print", p.toString)
      case _ => Nil
    }) ++ (proverLogFile match {
      case Some(p) => Seq("--proverLog", p.toString)
      case _ => Nil
    }) ++ options ++ Seq("-")

    carbon.parseCommandLine(carbonConfig)

    carbon.start()

    val plugins = SilverPluginManager(Some(Seq(
      "viper.silver.plugin.standard.termination.TerminationPlugin",
    ).mkString(":")))(carbon.reporter, getLogger("viper.silver.plugin").asInstanceOf[ch.qos.logback.classic.Logger], carbon.config)

    (carbon, plugins)
  }

  override def stopVerifier(verifier: Verifier): Unit = {
    verifier.stop()
  }
}
