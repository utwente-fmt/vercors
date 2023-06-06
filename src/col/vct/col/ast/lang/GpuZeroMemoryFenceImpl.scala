package vct.col.ast.lang

import vct.col.ast.GpuZeroMemoryFence
import vct.col.print.{Ctx, Doc, Text}

trait GpuZeroMemoryFenceImpl[G] { this: GpuZeroMemoryFence[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(value.toString)
}
