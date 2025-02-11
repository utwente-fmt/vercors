package vct.rewrite.rtos.freertosir

import vct.col.ast.CLocal
import vct.col.rewrite.Generation
import vct.rewrite.rtos.{ObjectInfo, Transformer}

case class EventGroup[O <: Generation](decl: Option[CLocal[O]])
    extends FreeRTOSConstruct[O] {
  override def convert(col_ir: Transformer[O], idx: Int): ObjectInfo[O] = ???
}
