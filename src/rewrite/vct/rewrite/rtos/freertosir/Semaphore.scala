package vct.rewrite.rtos.freertosir

import vct.col.ast._
import vct.col.ref.Ref

sealed trait Semaphore
case class BinarySemaphore(is_mutex: Boolean) extends Semaphore {
  def transform[N](scheduler_ref: Ref[N, Class[N]], release_event: Int): Class[N] = ???
}
case class RecursiveMutex() extends Semaphore {
  def transform[N](scheduler_ref: Ref[N, Class[N]], release_event: Int): Class[N] = ???
}