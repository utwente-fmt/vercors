package hre.debug

import hre.util.ScopedStack
import vct.result.VerificationError
import vct.result.VerificationError.SystemError

/**
 * Thread-local counter that can associate errors with counted causes.
 */
object TimeTravel {
  case class RepeatUntilCause(causeIndex: Long, exception: VerificationError) extends SystemError {
    override def text: String = exception.text
  }

  case class CauseWithBadEffect(effect: VerificationError) extends SystemError {
    override def text: String = s"This trace causes an error later on: ${effect.getMessage}"
  }

  private val causeIndex: ThreadLocal[Long] = ThreadLocal.withInitial(() => 0L)
  private val repeatingUntil: ScopedStack[(Long, VerificationError)] = ScopedStack()

  private def nextIndex(): Long = {
    val index = causeIndex.get()
    causeIndex.set(index + 1)
    index
  }

  def safelyRepeatable[T](f: => T): T = {
    val start = causeIndex.get()
    try {
      f
    } catch {
      case r @ RepeatUntilCause(idx, t) if idx >= start =>
        t.contexts ++= r.contexts
        causeIndex.set(start)
        repeatingUntil.having((idx, t)) { f }
    }
  }

  def cause[T](f: Long => T): T = {
    val idx = nextIndex()
    if(repeatingUntil.topOption.exists(_._1 == idx)) {
      throw CauseWithBadEffect(repeatingUntil.top._2)
    } else {
      f(idx)
    }
  }

  def badEffect[T](idx: Long, f: => Nothing): Nothing =
    try {
      f
    } catch {
      case t: VerificationError => throw RepeatUntilCause(idx, t)
    }
}
