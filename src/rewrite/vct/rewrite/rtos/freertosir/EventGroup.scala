package vct.rewrite.rtos.freertosir

import vct.col.ast.CLocal
import vct.col.rewrite.Generation
import vct.rewrite.rtos.{ObjectInfo, COLEncoder}

case class EventGroup[O <: Generation](decl: Option[CLocal[O]])
    extends FreeRTOSConstruct[O] {
  override def convert(col_ir: COLEncoder[O], idx: Int): ObjectInfo[O] = ???
}
