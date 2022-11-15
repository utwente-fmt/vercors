package viper.api.backend.silicon

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, Logger}
import ch.qos.logback.core.AppenderBase
import org.slf4j.LoggerFactory
import org.slf4j.LoggerFactory.getLogger
import vct.col.ast.{Expr, Node}
import vct.col.origin.Origin
import viper.api.Resources
import viper.api.backend.SilverBackend
import viper.silicon.logger.SymbExLogger
import viper.silver.plugin.SilverPluginManager
import viper.silver.reporter.Reporter
import viper.silver.verifier.Verifier

import java.nio.file.Path
import java.util.{Timer, TimerTask}
import scala.annotation.nowarn
import scala.collection.mutable
import scala.sys.ShutdownHookThread
import scala.util.matching.{Regex, UnanchoredRegex}

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
case class Silicon(
  z3Settings: Map[String, String] = Map.empty,
  z3Path: Path = Resources.getZ3Path,
  numberOfParallelVerifiers: Option[Int] = None,
  proverLogFile: Option[Path] = None,
  printQuantifierStatistics: Boolean = false,
  timeoutValue: Int = 30,
  options: Seq[String] = Nil,
) extends SilverBackend {

  var la: ConcurrentListAppender[ILoggingEvent] = null
  var nodeFromUniqueId: Map[Int, Node[_]] = Map()
  var shutdownHookThread: ShutdownHookThread = null
  var reportedQuantifiers = false
  var intermediatePrinterTimer: Timer = null

  override def createVerifier(reporter: Reporter, nodeFromUniqueId: Map[Int, Node[_]]): (viper.silicon.Silicon, SilverPluginManager) = {
    this.nodeFromUniqueId = nodeFromUniqueId

    if (printQuantifierStatistics) {
      val l = LoggerFactory.getLogger("viper.silicon.decider.Z3ProverStdIO").asInstanceOf[Logger]
      l.setLevel(Level.INFO)
      la = new ConcurrentListAppender[ILoggingEvent]()
      la.setName("quantifier-instantations-appender")
      la.start()
      l.setAdditive(false) // Prevent bubbling up
      l.addAppender(la)

      intermediatePrinterTimer = new Timer()
      intermediatePrinterTimer.schedule(new TimerTask {
        override def run(): Unit = shortQuantifierReport()
      }, 5000, 5000)

      shutdownHookThread = sys.addShutdownHook({
        longQuantifierReport()
        reportedQuantifiers = true
      })
    }

    val silicon = new viper.silicon.Silicon(reporter)

    val z3Config = z3Settings.map{case (k, v) => s"$k=$v"}.mkString(" ")

    var siliconConfig = Seq(
      "--assertTimeout", (timeoutValue*1000).toString,
      "--z3Exe", z3Path.toString,
      "--z3ConfigArgs", z3Config,
      "--ideModeAdvanced",
    )

    proverLogFile match {
      case Some(p) => siliconConfig ++= Seq("--z3LogFile", p.toString) // This should be changed to "proverLogFile" when updating to the new Viper version
      case _ => siliconConfig ++= Seq("--disableTempDirectory") // Otherwise do not make a temp dir (these two options are mutually exclusive)
    }

    numberOfParallelVerifiers match {
      case Some(n) =>
        siliconConfig ++= Seq("--numberOfParallelVerifiers", n.toString)
      case None =>
    }

    siliconConfig ++= options

    siliconConfig :+= "-"

    silicon.parseCommandLine(siliconConfig)

    SymbExLogger.setListenerProvider(_ => SiliconLogListener())

    silicon.start()

    val plugins = SilverPluginManager(Some(Seq(
      "viper.silver.plugin.standard.termination.TerminationPlugin",
    ).mkString(":")))(silicon.reporter, getLogger("viper.silver.plugin").asInstanceOf[ch.qos.logback.classic.Logger], silicon.config, null)

    (silicon, plugins)
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

  // TODO: Refactor this code to keep the parsed quantifier stats around, possibly only the biggest ones, only poll for new ones,
  //       and print ignored lines.
  //       Also, integrate with SymbExLogger/SiliconLogListener, for code  reuse and to get similar presentation/behaviour.
  // Parses z3 quantifier output into our own report data structure
  // Format info: https://github.com/Z3Prover/z3/blob/z3-4.8.6/src/smt/smt_quantifier.cpp#L173-L181
  val quantifierStatFormatR: UnanchoredRegex = raw"\[quantifier_instances]\s*(\S+)\s*:\s*(\S+)\s*:\s*(\S+)\s*:\s*(\S+)".r.unanchored
  val uniqueIdR: Regex = raw".*unique_id=(\d+)".r
  def getQuantifierInstanceReports(): Seq[QuantifierInstanceReport] = {
    la.getAll()
      .map(_.toString)
      .collect {
        case quantifierStatFormatR(qid, numInstances, maxGeneration, maxCost) =>
          val id = qid match {
            case uniqueIdR(intId) =>
              Right(nodeFromUniqueId(intId.toInt).asInstanceOf[Expr[_]])
            case _ => Left(qid)
          }
          QuantifierInstanceReport(id, numInstances.toInt, maxGeneration.toInt, maxCost.toInt)
      }
      .map(r => (r.e, r))
      .sortBy(_._2.instances)
      .toMap.values.toSeq
  }

  def longQuantifierReport(): Unit = {
    val reports = getQuantifierInstanceReports()
    if (reports.nonEmpty) {
      logger.info("Reporting quantifier instances statistics in descending order:")
      for (report <- reports.sortBy(_.instances).reverse) {
        report.e match {
          case Right(e) =>
            logger.info(e.o.messageInContext(
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
  }

  def shortQuantifierReport(): Unit = {
    val reports = getQuantifierInstanceReports()
    if (reports.nonEmpty) {
      logger.info("=== Quantifier instantiation statistics ===")
      val orderedReports = reports.sortBy(_.instances).reverse
      val cutReports = if (logger.underlying.isDebugEnabled) orderedReports else orderedReports.take(10)
      for (report <- cutReports) {
        report.e match {
          case Right(e) =>
            val o = e.o
            logger.info(s"${o.shortPosition}: inst: ${report.instances} (gen: ${report.maxGeneration}, cost: ${report.maxCost})")
          case Left(n) =>
            logger.info(s"$n: inst: ${report.instances} (gen: ${report.maxGeneration}, cost: ${report.maxCost})")
        }
      }
      if (cutReports.length < orderedReports.length) {
        logger.info(s"Omitting ${orderedReports.length - cutReports.length} other quantifiers...")
      }
    }
  }

  override def stopVerifier(verifier: Verifier): Unit = {
    verifier.stop()
    SymbExLogger.reset()

    if (printQuantifierStatistics) {
      intermediatePrinterTimer.cancel()
      shutdownHookThread.remove()
      if (!reportedQuantifiers) {
        longQuantifierReport()
      }
    }
  }
}
