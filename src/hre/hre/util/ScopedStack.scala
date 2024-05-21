package hre.util

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap}

case object ScopedStack {
  implicit class ScopedArrayBufferStack[T](stack: ScopedStack[ArrayBuffer[T]]) {
    def collect[R](f: => R): (Seq[T], R) = {
      val buf = ArrayBuffer[T]()
      val res = stack.having(buf)(f)
      (buf.toSeq, res)
    }
  }

  implicit class ScopedSetStack[T](stack: ScopedStack[mutable.Set[T]]) {
    def collect[R](f: => R): (mutable.Set[T], R) = {
      val buf = mutable.Set[T]()
      val res = stack.having(buf)(f)
      (buf, res)
    }
  }

  implicit class ScopedHashMapStack[K, V](stack: ScopedStack[HashMap[K,V]]){
    def collect[R](f: => R): (HashMap[K,V], R) = {
      val map = HashMap[K,V]()
      val res = stack.having(map)(f)
      (map, res)
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
  def foreach(f: T => Unit): Unit = stack.foreach(f)
  def toSeq: Seq[T] = stack.toSeq
  def contains(t: T): Boolean = find(_ == t).isDefined

  def having[R](x: T)(f: => R): R = {
    stack.push(x)
    try {
      f
    } finally {
      stack.pop()
    }
  }
}
