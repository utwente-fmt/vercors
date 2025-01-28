package hre.log

import ch.qos.logback.classic.{Level, Logger}
import hre.middleware.{Middleware, MiddlewareObject}
import org.slf4j.LoggerFactory

import java.util.ServiceLoader

object Logging extends MiddlewareObject[Logging] {
  var logLevels: Seq[(String, Verbosity)] = Nil

  def withLogLevels(logLevels: Seq[(String, Verbosity)]): this.type = {
    Logging.logLevels = logLevels
    Logging
  }

  override def apply(): Logging = Logging(logLevels)
}

case class Logging(logLevels: Seq[(String, Verbosity)]) extends Middleware {
  override protected def install(): Unit = {
    for ((key, logLevel) <- logLevels) {
      LoggerFactory.getLogger(key).asInstanceOf[Logger]
        .setLevel(logLevel.asLogback)
    }
  }

  override protected def uninstall(): Unit = {}
}
