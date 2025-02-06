package vct.rewrite.rtos.freertosir

import vct.rewrite.rtos.ObjectInfo

trait FreeRTOSConstruct[O] {
  def convert[N]: ObjectInfo[O, N]
}
