package vct.col.ast.lang

import vct.col.ast.GPULocal
import vct.col.print.{Ctx, Doc, Text}

trait GPULocalImpl[G] {
  this: GPULocal[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("__vercors_local_memory__")
}
