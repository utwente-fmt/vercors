package vct.main

import ch.qos.logback.classic.{Level, Logger}
import com.typesafe.scalalogging.LazyLogging
import hre.progress.Progress
import org.slf4j.LoggerFactory
import scopt.OParser
import vct.col.ast.Node
import vct.main.modes.Verify
import vct.main.stages.Transformation
import vct.options.types.{Mode, Verbosity}
import vct.options.Options
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

  /**
   * The main entry point of the VerCors verifier.
   *
   * Parses the options, and decides from there what to do.
   * The exit code of VerCors determines the verification result: zero means all proof goals succeeded.
   *
   * @param args The command line argument
   */
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

  /**
   * Decide what to do from the parsed options.
   *
   * If the help flag is enabled, the usage is printed.
   * Sets up the logging levels in slf4j, and enables progress logging.
   * Finally the method switches on the mode enabled in the options, deferring to the appropriate object in [[vct.main.modes]]
   *
   * @param options The parsed command line arguments. VerCors is not meant to be invoked with e.g. a tweaked [[Options]]
   *                object. Rather, refer to [[Verify.verifyWithSilicon]] or [[vct.main.stages.Stages]]
   * @return The exit code, zero on verification success.
   */
  def runOptions(options: Options): Int = {
    if(options.help) {
      println(OParser.usage(Options.parser(hide = !options.showHidden)))
      return 0
    }

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

    Progress.install(options.progress)

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