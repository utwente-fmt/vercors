package hre.util

import scala.collection.mutable

case class ScopedStack[T]() extends mutable.Stack[T] {
  def having[R](x: T)(f: => R): R = {
    push(x)
    val res = f
    pop()
    res
  }
}
