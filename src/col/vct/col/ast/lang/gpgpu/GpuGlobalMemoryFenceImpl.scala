package vct.col.ast.lang.gpgpu

import vct.col.ast.GpuGlobalMemoryFence
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.GpuGlobalMemoryFenceOps

trait GpuGlobalMemoryFenceImpl[G] extends GpuGlobalMemoryFenceOps[G] {
  this: GpuGlobalMemoryFence[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("__vercors_global_mem_fence__")
}
