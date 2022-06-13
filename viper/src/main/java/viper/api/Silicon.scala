package viper.api

import ch.qos.logback.classic.Logger
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import hre.config.Configuration
import org.slf4j.LoggerFactory
import viper.silicon.logger.SymbExLogger
import viper.silver.reporter.Reporter
import viper.silver.verifier.Verifier
import java.io.ByteArrayOutputStream
import java.nio.file.Path
import scala.annotation.nowarn
import scala.jdk.CollectionConverters._

@nowarn("any") // due to be removed
case class Silicon(z3Settings: Map[String, String] = Map.empty, z3Path: Path = Resources.getZ3Path, numberOfParallelVerifiers: Option[Int] = None, logLevel: Option[String] = None, proverLogFile: Option[Path] = None) extends SilverBackend {

  var la: ListAppender[ILoggingEvent] = null

  override def createVerifier(reporter: Reporter): viper.silicon.Silicon = {
    val silicon = new viper.silicon.Silicon(reporter)

    val z3Config = '"' + z3Settings.map{case (k, v) => s"$k=$v"}.mkString(" ") + '"'

    var siliconConfig = Seq(
      "--z3Exe", z3Path.toString,
      "--z3ConfigArgs", z3Config,
    )

    val l = LoggerFactory.getLogger("viper.silicon.decider.Z3ProverStdIO").asInstanceOf[Logger]
    l.detachAndStopAllAppenders()
    la = new ListAppender[ILoggingEvent]()
    la.setName("quantifier-instantations-appender")
    la.start()
    l.setAdditive(false) // Prevent bubbling up
    l.addAppender(la)

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
    /*TODO (RR): Need to gather all quantifier names here, and map them to origins.
        Then when submit is done, we map as many of the quantifier info lines back to origins, print a table of that,
        and then print a table of all the shortened names.

        Except: quant info printing is silicon specific, yet this is SilverBackend!
     */
    // `[quantifier_instances] "<quantifier_id>" : <instances> : <maximum generation> : <maximum cost>`.
    val l = LoggerFactory.getLogger("viper.silicon.decider.Z3ProverStdIO").asInstanceOf[Logger]
    val la = l.getAppender("quantifier-instantations-appender").asInstanceOf[ListAppender[ILoggingEvent]]
    println("--- RESULTS ---")
    for (m <- la.list.asScala) {
      println(m)
    }
  }
}
