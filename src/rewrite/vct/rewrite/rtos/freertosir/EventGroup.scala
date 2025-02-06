package vct.rewrite.rtos.freertosir

import vct.col.ast.CLocal
import vct.rewrite.rtos.{ObjectInfo, Transformer}

case class EventGroup[O, N](decl: Option[CLocal[O]])
    extends FreeRTOSConstruct[O, N] {
  override def convert(col_ir: Transformer[O, N], idx: Int): ObjectInfo[O, N] = ???
}
