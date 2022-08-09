package hre.util

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case object ScopedStack {
  implicit class ScopedArrayBufferStack[T](stack: ScopedStack[ArrayBuffer[T]]) {
    def collect[R](f: => R): (Seq[T], R) = {
      val buf = ArrayBuffer[T]()
      val res = stack.having(buf)(f)
      (buf.toSeq, res)
    }
  }
}

case class ScopedStack[T]() {
  private val stacks: ThreadLocal[mutable.Stack[T]] = ThreadLocal.withInitial(() => mutable.Stack())

  def stack: mutable.Stack[T] = stacks.get()

  def isEmpty: Boolean = stack.isEmpty
  def nonEmpty: Boolean = stack.nonEmpty
  def push(t: T): Unit = stack.push(t)
  def pop(): T = stack.pop()
  def top: T = stack.top
  def topOption: Option[T] = stack.headOption
  def find(f: T => Boolean): Option[T] = stack.find(f)
  def exists(f: T => Boolean): Boolean = stack.exists(f)
  def toSeq: Seq[T] = stack.toSeq

  def having[R](x: T)(f: => R): R = {
    stack.push(x)
    try {
      f
    } finally {
      stack.pop()
    }
  }
}
