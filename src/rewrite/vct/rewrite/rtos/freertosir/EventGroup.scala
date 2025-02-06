package vct.rewrite.rtos.freertosir

import vct.col.ast.CLocal
import vct.rewrite.rtos.ObjectInfo

case class EventGroup[O](decl: Option[CLocal[O]]) extends FreeRTOSConstruct[O] {
  override def convert[N]: ObjectInfo[O, N] = ???
}