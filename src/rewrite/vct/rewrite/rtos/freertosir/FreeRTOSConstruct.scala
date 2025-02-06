package vct.rewrite.rtos.freertosir

import vct.rewrite.rtos.{ObjectInfo, Transformer}

trait FreeRTOSConstruct[O, N] {
  def convert(col_ir: Transformer[O, N], idx: Int): ObjectInfo[O, N]
}
