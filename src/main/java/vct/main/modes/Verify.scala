package vct.main.modes

import com.typesafe.scalalogging.LazyLogging
import vct.options.Options
import hre.io.Readable
import sun.misc.{Signal, SignalHandler}
import vct.col.origin.{BlameCollector, TableEntry, VerificationFailure}
import vct.col.rewrite.bip.BIP
import vct.col.rewrite.bip.BIP.Standalone.VerificationReport
import vct.main.Main.{EXIT_CODE_ERROR, EXIT_CODE_SUCCESS, EXIT_CODE_VERIFICATION_FAILURE}
import vct.main.stages.Stages
import vct.options.types.PathOrStd
import vct.parsers.transform.ConstantBlameProvider
import vct.result.VerificationError
import viper.api.backend.silicon.SiliconLogListener
import viper.silicon.logger.SymbExLogger

case object Verify extends LazyLogging {
  def verifyWithSilicon(inputs: Seq[Readable]): Either[VerificationError, (Seq[VerificationFailure], VerificationReport)] = {
    val collector = BlameCollector()
    val bipResults = BIP.VerificationResults()
    val stages = Stages.silicon(ConstantBlameProvider(collector), bipResults)
    logger.debug(stages.toString)
    stages.run(inputs) match {
      case Left(error) => Left(error)
      case Right(()) => Right((collector.errs.toSeq, bipResults.toStandalone()))
    }
  }

  def verifyWithCarbon(inputs: Seq[Readable]): Either[VerificationError, (Seq[VerificationFailure], VerificationReport)] = {
    val collector = BlameCollector()
    val bipResults = BIP.VerificationResults()
    val stages = Stages.carbon(ConstantBlameProvider(collector), bipResults)
    logger.debug(stages.toString)
    stages.run(inputs) match {
      case Left(error) => Left(error)
      case Right(()) => Right((collector.errs.toSeq, bipResults.toStandalone()))
    }
  }

  def verifyWithOptions(options: Options, inputs: Seq[Readable]): Either[VerificationError, (Seq[VerificationFailure], BIP.Standalone.VerificationReport)] = {
    val collector = BlameCollector()
    val bipResults = BIP.VerificationResults()
    val stages = Stages.ofOptions(options, ConstantBlameProvider(collector), bipResults)
    logger.debug("Stages: " ++ stages.flatNames.map(_._1).mkString(", "))
    val r = stages.run(inputs) match {
      case Left(error) => Left(error)
      case Right(()) => Right((collector.errs.toSeq, bipResults.toStandalone()))
    }
    val x = 3
    r
  }

  /**
   * Runs a normal verification run from the command line options.
   *
   * After some setup, most of the meat is in [[Stages.ofOptions]].
   *
   * @param options The command line options.
   * @return The exit code, zero on verification success.
   */
  def runOptions(options: Options): Int = {
    try {
      // Wrapped in try because this seems to crash on windows
      Signal.handle(new Signal("USR1"), _ => SymbExLogger.memberList.synchronized {
        SymbExLogger.memberList.toSeq.map(_.listener).collect { case l: SiliconLogListener => l } match {
          case Nil => logger.warn("Silicon is idle.")
          case listeners => listeners.foreach(_.printDetailedState())
        }
      })
    } catch {
      // Could not register USR1 debug hook. This is expected behaviour on windows.
      case _: IllegalArgumentException =>
    }

    verifyWithOptions(options, options.inputs) match {
      case Left(err) =>
        logger.error(err.text)
        EXIT_CODE_ERROR
      case Right((Nil, report)) =>
        logger.info("Verification completed successfully.")
        friendlyHandleBipReport(report, options.bipReportFile)
        EXIT_CODE_SUCCESS
      case Right((fails, report)) =>
        if(options.more || fails.size <= 2) fails.foreach(fail => logger.error(fail.desc))
        else logger.error(TableEntry.render(fails.map(_.asTableEntry)))
        friendlyHandleBipReport(report, options.bipReportFile)
        EXIT_CODE_VERIFICATION_FAILURE
    }
  }

  def friendlyHandleBipReport(report: VerificationReport, path: Option[PathOrStd]): Unit = (report, path) match {
    case (report, Some(path)) if report.nonEmpty() => path.write(w => w.write(report.toJson()))
    case (report, None) if report.nonEmpty() => logger.warn("JavaBIP verification report was produced, but no output path was specified. Use `--bip-report-file` to specify an output. See `--help` for more info.")
    case (report, None) if report.isEmpty() =>
  }
}
