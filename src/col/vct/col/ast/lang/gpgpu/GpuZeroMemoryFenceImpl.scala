package vct.col.ast.lang.gpgpu

import vct.col.ast.GpuZeroMemoryFence
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.GpuZeroMemoryFenceOps

trait GpuZeroMemoryFenceImpl[G] extends GpuZeroMemoryFenceOps[G] {
  this: GpuZeroMemoryFence[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(value.toString)
}
