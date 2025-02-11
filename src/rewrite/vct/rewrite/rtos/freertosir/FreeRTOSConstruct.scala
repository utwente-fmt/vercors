package vct.rewrite.rtos.freertosir

import vct.col.rewrite.{Generation, Rewritten}
import vct.rewrite.rtos.{ObjectInfo, Transformer}

trait FreeRTOSConstruct[O <: Generation] {
  type N = Rewritten[O]
  def convert(col_ir: Transformer[O], idx: Int): ObjectInfo[O]
}
