package vct.main

import ch.qos.logback.classic.{Level, Logger}
import com.typesafe.scalalogging.LazyLogging
import hre.io.{CollectString, InterruptibleStdin, Watch}
import hre.log.Logging
import hre.middleware.Middleware
import hre.perf.Profile
import hre.progress.{Layout, Progress, TaskRegistry}
import hre.util.ThreadWatchdog
import org.slf4j.LoggerFactory
import scopt.OParser
import vct.col.ast.Node
import vct.debug.CrashReport
import vct.main.modes.{CFG, VeSUV, Verify, VeyMont}
import vct.main.stages.Transformation
import vct.options.Options
import vct.options.types.Mode
import vct.result.VerificationError.UserError

import scala.util.control.NonFatal

case object Main extends LazyLogging {
  val EXIT_CODE_SUCCESS = 0
  val EXIT_CODE_VERIFICATION_FAILURE = 1
  val EXIT_CODE_ERROR = 2

  case class TemporarilyUnsupported(feature: String, examples: Seq[Node[_]])
      extends UserError {
    override def code: String = "unsupported"
    override def text: String =
      examples.head.o
        .messageInContext(s"The feature `$feature` is temporarily unsupported.")
  }

  /** The main entry point of the VerCors verifier.
    *
    * Parses the options, and decides from there what to do. The exit code of
    * VerCors determines the verification result: zero means all proof goals
    * succeeded.
    *
    * @param args
    *   The command line argument
    */
  def main(args: Array[String]): Unit =
    try {
      Options.parse(args) match {
        case None => System.exit(EXIT_CODE_ERROR)
        case Some(options) =>
          try { System.exit(runOptions(options)) }
          catch {
            case NonFatal(err) =>
              logger.error(CollectString(stream => err.printStackTrace(stream)))
              logger.error("!*!*!*!*!*!*!*!*!*!*!*!")
              logger.error("! VerCors has crashed !")
              logger.error("!*!*!*!*!*!*!*!*!*!*!*!")
              logger.error("")
              logger.error("Please report this as a bug here:")
              logger.error(CrashReport.makeGithubLink(err, args, options))
              System.exit(EXIT_CODE_ERROR)
          }
      }
    } catch {
      case t: Throwable =>
        logger.error(s"Unrecoverable error: ${t.getMessage}", t)
        throw t
    }

  /** Decide what to do from the parsed options.
    *
    * If the help flag is enabled, the usage is printed. Sets up the logging
    * levels in slf4j, and enables progress logging. Finally the method switches
    * on the mode enabled in the options, deferring to the appropriate object in
    * [[vct.main.modes]]
    *
    * @param options
    *   The parsed command line arguments. VerCors is not meant to be invoked
    *   with e.g. a tweaked [[Options]] object. Rather, refer to
    *   [[Verify.verifyWithSilicon]] or [[vct.main.stages.Stages]]
    * @return
    *   The exit code, zero on verification success.
    */
  def runOptions(options: Options): Int = {
    if (options.help) {
      println(OParser.usage(Options.parser(hide = !options.showHidden)))
      return 0
    }

    Middleware.using(
      true -> InterruptibleStdin,
      true -> Logging.withLogLevels(options.logLevels),
      true -> ThreadWatchdog,
      true -> Layout.withForceProgress(options.progress),
    ) {
      Watch.booleanWithWatch(options.watch, default = EXIT_CODE_SUCCESS) {
        Middleware.using(
          true -> TaskRegistry,
          true -> Progress,
          options.profile -> Profile,
        )(runMode(options.mode, options))
      }
    }
  }

  def runMode(mode: Mode, options: Options): Int =
    options.mode match {
      case Mode.Verify =>
        logger.info(s"Starting verification at ${hre.util.Time.formatTime()}")
        Verify.runOptions(options)
      case Mode.HelpVerifyPasses =>
        logger.info("Available passes:")
        Transformation.ofOptions(options).passes.foreach { pass =>
          logger.info(s" - ${pass.key}")
          logger.info(s"    ${pass.desc}")
        }
        EXIT_CODE_SUCCESS
      case Mode.VeyMont => VeyMont.runOptions(options)
      case Mode.VeSUV =>
        logger.info("Starting transformation")
        VeSUV.runOptions(options)
      case Mode.CFG =>
        logger.info("Starting control flow graph transformation")
        CFG.runOptions(options)
    }
}
