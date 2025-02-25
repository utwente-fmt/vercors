package vct.parsers.transform.systemctocol.util

import scala.annotation.varargs

object Seqs {
  def empty[T](): Seq[T] = Seq.empty

  def singleton[T](t: T): Seq[T] = Seq(t)

  @varargs
  def make[T](ts: T*): Seq[T] = Seq(ts: _*)
}
