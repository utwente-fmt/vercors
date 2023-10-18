package vct.result

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

sealed abstract class VerificationError extends RuntimeException {
  def text: String

  override def getMessage: String = text

  val contexts: ArrayBuffer[VerificationError.Context] = ArrayBuffer()

  def getContext[T](implicit tag: ClassTag[T]): Option[T] = {
    contexts.collectFirst { case x: T => x }
  }
}

object VerificationError {
  /* Malformed input, unsupported features, and any other documented deficiencies in VerCors */
  abstract class UserError extends VerificationError {
    def code: String
  }

  /* Some state was reached that was not expected. A SystemError is *always* a bug. If it is an expected failure, it
   * should be a (documented) UserError. */
  abstract class SystemError extends VerificationError

  case class Unreachable(text: String) extends SystemError

  trait Context

  def context[T](ctx: Context)(f: => T): T = {
    try { f }
    catch {
      case e: VerificationError =>
        e.contexts += ctx
        throw e
    }
  }
}
