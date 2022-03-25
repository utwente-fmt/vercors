package vct.main

import ch.qos.logback.classic.{Level, Logger}
import com.typesafe.scalalogging.LazyLogging
import org.slf4j.LoggerFactory
import vct.col.ast.Node
import vct.main.modes.Verify
import vct.main.stages.Transformation
import vct.options.{Mode, Options, Verbosity}
import vct.result.VerificationError.UserError

case object Main extends LazyLogging {
  val EXIT_CODE_SUCCESS = 0
  val EXIT_CODE_VERIFICATION_FAILURE = 1
  val EXIT_CODE_ERROR = 2

  case class TemporarilyUnsupported(feature: String, examples: Seq[Node[_]]) extends UserError {
    override def code: String = "unsupported"
    override def text: String =
      examples.head.o.messageInContext(
        s"The feature `$feature` is temporarily unsupported.")
  }

  def main(args: Array[String]): Unit = try {
    Options.parse(args) match {
      case None => System.exit(EXIT_CODE_ERROR)
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
        logger.info("Available passes:")
        Transformation.ofOptions(options).passes.foreach { pass =>
          logger.info(s" - ${pass.key}")
          logger.info(s"    ${pass.desc}")
        }
        EXIT_CODE_SUCCESS
      case Mode.VeyMont => ???
      case Mode.BatchTest => ???
    }
  }
}
