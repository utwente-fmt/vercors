package viper.api
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, Logger}
import ch.qos.logback.core.OutputStreamAppender
import com.google.common.base.Utf8
import hre.config.Configuration
import org.slf4j.LoggerFactory
import viper.silicon.logger.SymbExLogger
import viper.silver.plugin.PluginAwareReporter
import viper.silver.reporter.Reporter
import viper.silver.verifier.Verifier

import java.io.ByteArrayOutputStream
import java.nio.charset.Charset
import java.nio.file.Path
import scala.annotation.nowarn
import scala.io.Codec

@nowarn("any") // due to be removed
case class Silicon(z3Settings: Map[String, String] = Map.empty, z3Path: Path = Resources.getZ3Path, numberOfParallelVerifiers: Option[Int] = None, logLevel: Option[String] = None, proverLogFile: Option[Path] = None) extends SilverBackend {
  var os: ByteArrayOutputStream

  override def createVerifier(reporter: Reporter): viper.silicon.Silicon = {
    val silicon = new viper.silicon.Silicon(reporter)

    val z3Config = '"' + z3Settings.map{case (k, v) => s"$k=$v"}.mkString(" ") + '"'

    var siliconConfig = Seq(
      "--z3Exe", z3Path.toString,
      "--z3ConfigArgs", z3Config,
    )

    val l = LoggerFactory.getLogger("viper.silicon.decider.ProverStdIO").asInstanceOf[Logger]
    l.setLevel(Level.INFO)
    l.detachAndStopAllAppenders()
    val osa = new OutputStreamAppender[ILoggingEvent]()
    os = new ByteArrayOutputStream();
    osa.setOutputStream(os)
    l.addAppender(osa)

    proverLogFile match {
      case Some(p) => siliconConfig ++= Seq("--z3LogFile", p.toString) // This should be changed to "proverLogFile" when updating to the new Viper version
      case _ => siliconConfig ++= Seq("--disableTempDirectory") // Otherwise do not make a temp dir (these two options are mutually exclusive)
    }

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

    // TODO (RR): map quantifier insts with known names back to origins?
    val proverStdIOLog = os.toString(Charset.forName("UTF-8"))
    println(proverStdIOLog)
  }
}
