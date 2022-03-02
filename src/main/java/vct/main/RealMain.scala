package vct.main

import ch.qos.logback.classic.{Level, Logger}
import com.typesafe.scalalogging.LazyLogging
import org.slf4j.LoggerFactory
import vct.options.{Mode, Options}
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
    if(!options.backendDebug)
      LoggerFactory.getLogger("viper").asInstanceOf[Logger].setLevel(Level.OFF)

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
      case Mode.VeyMont => ???
      case Mode.BatchTest => ???
    }
  } catch {
    case e: Throwable =>
      logger.error("Fatal error", e)
  }
}
