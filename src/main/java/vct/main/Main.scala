package vct.main

import ch.qos.logback.classic.{Level, Logger}
import com.typesafe.scalalogging.LazyLogging
import org.slf4j.LoggerFactory
import vct.main.modes.Verify
import vct.options.{Mode, Options, Verbosity}

case object Main extends LazyLogging {
  val EXIT_CODE_SUCCESS = 0

  def main(args: Array[String]): Unit = try {
    Options.parse(args) match {
      case None => // usage was printed
      case Some(options) => System.exit(runOptions(options))
    }
  } catch {
    case t: Throwable =>
      logger.error(s"Unrecoverable error: ${t.getMessage}", t)
      throw t
  }

  def runOptions(options: Options): Int = {
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
        Verify.runOptions(options)
      case Mode.HelpVerifyPasses =>
        Vercors(options).helpPasses()
        EXIT_CODE_SUCCESS
      case Mode.VeyMont => ???
      case Mode.BatchTest => ???
    }
  }
}
