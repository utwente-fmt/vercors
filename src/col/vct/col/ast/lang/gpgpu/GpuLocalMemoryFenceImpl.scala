package vct.col.ast.lang.gpgpu

import vct.col.ast.GpuLocalMemoryFence
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.GpuLocalMemoryFenceOps

trait GpuLocalMemoryFenceImpl[G] extends GpuLocalMemoryFenceOps[G] {
  this: GpuLocalMemoryFence[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("__vercors_local_mem_fence__")
}
