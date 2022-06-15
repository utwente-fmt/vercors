package viper.api

import ch.qos.logback.classic.Logger
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.AppenderBase
import ch.qos.logback.core.read.ListAppender
import hre.config.Configuration
import hre.io.Writeable
import org.slf4j.LoggerFactory
import vct.col.ast.{Exists, Expr, Forall, Node, Program, Starall}
import vct.col.origin.Origin
import viper.silicon.logger.SymbExLogger
import viper.silver.reporter.Reporter
import viper.silver.verifier.Verifier

import java.io.ByteArrayOutputStream
import java.nio.file.Path
import java.util
import java.util.{Timer, TimerTask}
import scala.annotation.nowarn
import scala.jdk.CollectionConverters._
import scala.sys.ShutdownHookThread
import scala.collection.mutable

final class ConcurrentListAppender[E] extends AppenderBase[E] {
  var es: mutable.ArrayBuffer[E] = new mutable.ArrayBuffer[E]()

  override def append(e: E): Unit = {
    this.synchronized {
      es.addOne(e)
    }
  }

  def getAll(): Seq[E] = {
    this.synchronized {
      es.clone().toSeq
    }
  }
}

@nowarn("any") // due to be removed
case class Silicon(z3Settings: Map[String, String] = Map.empty, z3Path: Path = Resources.getZ3Path, numberOfParallelVerifiers: Option[Int] = None, logLevel: Option[String] = None, proverLogFile: Option[Path] = None) extends SilverBackend {

  var la: ConcurrentListAppender[ILoggingEvent] = null
  var qmap: Map[String, Expr[_]] = Map()
  var shutdownHookThread: ShutdownHookThread = null
  var reportedQuantifiers = false
  var intermediatePrinterTimer: Timer = new Timer()

  override def createVerifier(reporter: Reporter): viper.silicon.Silicon = {
    val silicon = new viper.silicon.Silicon(reporter)

    val z3Config = '"' + z3Settings.map{case (k, v) => s"$k=$v"}.mkString(" ") + '"'

    var siliconConfig = Seq(
      "--z3Exe", z3Path.toString,
      "--z3ConfigArgs", z3Config,
    )

    val l = LoggerFactory.getLogger("viper.silicon.decider.Z3ProverStdIO").asInstanceOf[Logger]
    la = new ConcurrentListAppender[ILoggingEvent]()
    la.setName("quantifier-instantations-appender")
    la.start()
    l.setAdditive(false) // Prevent bubbling up
    l.addAppender(la)

    intermediatePrinterTimer.schedule(new TimerTask {
      override def run(): Unit = shortQuantifierReport()
    }, 10 * 1000, 10 * 1000)

  shutdownHookThread = sys.addShutdownHook({
      longQuantifierReport()
      reportedQuantifiers = true
    })

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

  override def submit(colProgram: Program[_], output: Option[Writeable]): Unit = {
    val allQuantifiers: Seq[Expr[_]] = colProgram.transSubnodes.collect({
      case s: Starall[_] => s
      case f: Forall[_] => f
      case e: Exists[_] => e
    })
    qmap = allQuantifiers.map { q => (q.o.preferredName, q) } .toMap
    super.submit(colProgram, output)
  }

  // See: https://github.com/Z3Prover/z3/issues/4522
  /* Paraphrased Nikolaj's answer:
      > Generation is associated with the number of quantifier instantiations that were used to produce a given quantifier
      > instantiation. This is to understand which instantiations trigger other instantiations. For example, if you have
      > a quantifier forall x . p(x) => p(x + 1), and fact p(0). Then p(1) has generation 1, p(2) has generation 2, etc.
      >
      > Cost is associated with a heuristic that gives weights to quantifier instantiations. There is a default cost function
      > that gets applied to quantifiers and it is also possible to define cost functions through configuration. Roughly,
      > the cost of a quantifier instantiation should correspond to the preference of when to instantiate it.
   */
  case class QuantifierInstanceReport(e: Either[String, Expr[_]], instances: Int, maxGeneration: Int, maxCost: Int)

  def getQuantifierInstanceReports(): Seq[QuantifierInstanceReport] = {
    la.getAll()
      .map(_.toString)
      .filter(_.contains("quantifier_instances"))
      .map { m =>
        val msg = m.split("\\[quantifier_instances\\]")(1)
        val chunks = msg.split(':')
        val e = qmap.get(chunks(0).strip().replace("prog.l", ""))
          .toRight(chunks(0).strip())
        QuantifierInstanceReport(e, chunks(1).strip().toInt, chunks(2).strip().toInt, chunks(3).strip().toInt)
      }
      // Remove duplicates by only keeping the log entry with the higest number of instances
      .map(r => (r.e, r))
      .sortBy(_._2.instances)
      .toMap.values.toSeq
  }

  def longQuantifierReport(): Unit = {
    val reports = getQuantifierInstanceReports()
    if (reports.nonEmpty) {
      logger.info("Reporting quantifier instances statistics in descending order:")
    }
    for (report <- reports.sortBy(_.instances).reverse) {
      report.e match {
        case Right(e) =>
          val o = e.o
          logger.info(o.messageInContext(
            s"instances: ${report.instances} (gen: ${report.maxGeneration}, cost: ${report.maxCost})"))
        case Left(n) =>
          logger.info(
            s"""${Origin.BOLD_HR}Backend quantifier: $n
               |instances: ${report.instances} (gen: ${report.maxGeneration}, cost: ${report.maxCost})
               |${Origin.BOLD_HR}""".stripMargin
          )
      }
    }
  }

  def shortQuantifierReport(): Unit = {
    val reports = getQuantifierInstanceReports()

    if (reports.nonEmpty) {
      logger.info("Reporting quantifier instances statistics in descending order:")
    }
    for (report <- reports.sortBy(_.instances).reverse) {
      report.e match {
        case Right(e) =>
          val o = e.o
          logger.info(s"${o.toString}: inst: ${report.instances} (gen: ${report.maxGeneration}, cost: ${report.maxCost})")
        case Left(n) =>
          logger.info(s"$n: inst: ${report.instances} (gen: ${report.maxGeneration}, cost: ${report.maxCost})")
      }
    }
  }

  override def stopVerifier(verifier: Verifier): Unit = {
    verifier.stop()
    SymbExLogger.reset()
    intermediatePrinterTimer.cancel()

    shutdownHookThread.remove()
    if (!reportedQuantifiers) {
      longQuantifierReport()
      logger.info("#####")
      shortQuantifierReport()
    }
  }
}
