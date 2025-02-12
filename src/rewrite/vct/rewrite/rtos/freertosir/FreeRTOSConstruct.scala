package vct.rewrite.rtos.freertosir

import vct.col.rewrite.{Generation, Rewritten}
import vct.rewrite.rtos.{ObjectInfo, COLEncoder}

trait FreeRTOSConstruct[O <: Generation] {
  type N = Rewritten[O]
  def convert(col_ir: COLEncoder[O], idx: Int): ObjectInfo[O]
}
