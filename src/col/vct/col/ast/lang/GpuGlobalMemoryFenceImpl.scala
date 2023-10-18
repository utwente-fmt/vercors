package vct.col.ast.lang

import vct.col.ast.GpuGlobalMemoryFence
import vct.col.print.{Ctx, Doc, Text}

trait GpuGlobalMemoryFenceImpl[G] {
  this: GpuGlobalMemoryFence[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("__vercors_global_mem_fence__")
}
