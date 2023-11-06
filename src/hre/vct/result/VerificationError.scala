package vct.result

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.control.NonFatal

sealed abstract class VerificationError extends RuntimeException {
  def text: String

  override def getMessage: String = text

  val contexts: ArrayBuffer[VerificationError.Context] = ArrayBuffer()

  def context[T](implicit tag: ClassTag[T]): Option[T] = {
    contexts.collectFirst {
      case x : T =>
        x
    }
  }

  def getContext[T](implicit tag: ClassTag[T]): T = context[T].get

  def messageContext(message: String, backupContext: String => String = identity): String =
    // PB: note: the innermost context is added to contexts first, so we end up trying to use the most specific context
    // for rendering the message context first, which is what we want.
    contexts.foldLeft[Option[String]](None) {
      case (Some(messageInContext), _) => Some(messageInContext)
      case (None, ctx) => ctx.tryMessageContext(message, this)
    } getOrElse backupContext(message)
}

object VerificationError {
  /* Malformed input, unsupported features, and any other documented deficiencies in VerCors */
  abstract class UserError extends VerificationError {
    def code: String
  }

  /* Some state was reached that was not expected. A SystemError is *always* a bug. If it is an expected failure, it
   * should be a (documented) UserError. */
  abstract class SystemError extends VerificationError

  case class Unreachable(reason: String) extends SystemError {
    override def text: String = messageContext(reason)
  }

  case class Crash(cause: Throwable) extends SystemError {
    initCause(cause)

    override def text: String =
      messageContext("VerCors crashed near this position. Cause follows:" + cause.getStackTrace.mkString("\n"))
  }

  trait Context {
    def tryMessageContext(message: String, err: VerificationError): Option[String] = None
  }

  def withContext[T](ctx: Context)(f: => T): T = {
    try {
      f
    } catch {
      case e : VerificationError =>
        e.contexts += ctx
        throw e
      case NonFatal(e) =>
        // PB: if you clicked here, scoll down to the exception cause :)
        val crash = Crash(e)
        crash.contexts += ctx
        throw crash
    }
  }
}