package hre.debug

import hre.util.ScopedStack
import vct.result.VerificationError.SystemError

/**
 * Thread-local counter that can associate errors with counted causes.
 */
object TimeTravel {
  case class RepeatUntilCause(causeIndex: Long, exception: Throwable) extends RuntimeException

  case class CauseWithBadEffect(effect: Throwable) extends SystemError {
    override def text: String = s"This trace causes an error later on: ${effect.getMessage}"
  }

  private val causeIndex: ThreadLocal[Long] = ThreadLocal.withInitial(() => 0L)
  private val repeatingUntil: ScopedStack[(Long, Throwable)] = ScopedStack()

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
      case RepeatUntilCause(idx, t) if idx >= start =>
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
      case t: Throwable => throw RepeatUntilCause(idx, t)
    }
}
