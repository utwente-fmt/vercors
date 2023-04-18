package vct.col.ast.lang

import vct.col.ast.GpuLocalMemoryFence
import vct.col.print.{Ctx, Doc, Text}

trait GpuLocalMemoryFenceImpl[G] { this: GpuLocalMemoryFence[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("__vercors_local_mem_fence__")
}
