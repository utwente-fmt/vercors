package hre.util

import hre.middleware.{Middleware, MiddlewareObject}
import org.slf4j.LoggerFactory

object ThreadWatchdog extends MiddlewareObject[ThreadWatchdog]

case class ThreadWatchdog() extends Middleware {
  private val logger = LoggerFactory.getLogger(classOf[ThreadWatchdog])

  override protected def install(): Unit = {}

  override def uninstallOnShutdown(): Unit = {}

  override protected def uninstall(): Unit =
    synchronized {
      val thisThread = Thread.currentThread()
      Thread.getAllStackTraces.keySet().stream().filter(_ != thisThread)
        .filter(!_.isDaemon).forEach { thread =>
          val tg = Option(thread.getThreadGroup)
          logger.warn(
            s"Non-daemon thread ${tg.map(_.getName)
                .getOrElse("UnknownThreadGroup")}.${thread.getName} (#${thread.getId}) is still running"
          )
          logger.warn(
            "Due to this VerCors will not exit and sit idly until that thread is done. You may want to stop the program manually."
          )
        }
    }
}
