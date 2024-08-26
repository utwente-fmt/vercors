package hre.io

import hre.middleware.{Middleware, MiddlewareObject}

import java.io.InputStream

object InterruptibleStdin extends MiddlewareObject[InterruptibleStdin]

case class InterruptibleStdin() extends Middleware {
  private var previousStdin: InputStream = null

  override protected def install(): Unit = {
    previousStdin = System.in
    System.setIn(new InterruptibleInputStream(System.in))
  }

  override protected def uninstall(): Unit = System.setIn(previousStdin)
}
