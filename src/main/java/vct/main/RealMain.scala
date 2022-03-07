package vct.main

import ch.qos.logback.classic.{Level, Logger}
import com.typesafe.scalalogging.LazyLogging
import org.slf4j.LoggerFactory
import vct.options.{Mode, Options, Verbosity}
import vct.result.VerificationResult

case object RealMain extends LazyLogging {
  def main(args: Array[String]): Unit = try {
    Options.parse(args) match {
      case None => // usage was printed
      case Some(options) => selectMode(options)
    }
  } catch {
    case err: VerificationResult.SystemError =>
      println(err.text)
      err.printStackTrace()
  }

  def selectMode(options: Options): Unit = try {
    for((key, logLevel) <- options.logLevels) {
      LoggerFactory.getLogger(key).asInstanceOf[Logger].setLevel(logLevel match {
        case Verbosity.Off => Level.OFF
        case Verbosity.Error => Level.ERROR
        case Verbosity.Warning => Level.WARN
        case Verbosity.Info => Level.INFO
        case Verbosity.Debug => Level.DEBUG
        case Verbosity.Trace => Level.TRACE
        case Verbosity.All => Level.ALL
      })
    }

    options.mode match {
      case Mode.Verify =>
        logger.info("Starting verification")
        Vercors(options).go() match {
          case error: VerificationResult.UserError =>
            logger.error(error.text)
          case error: VerificationResult.SystemError =>
            logger.error(error.text, error)
          case VerificationResult.Ok =>
            logger.info("Verifcation completed normally.")
        }
      case Mode.HelpVerifyPasses =>
        Vercors(options).helpPasses()
      case Mode.VeyMont => ???
      case Mode.BatchTest => ???
    }
  } catch {
    case e: Throwable =>
      logger.error("Fatal error", e)
  }
}
