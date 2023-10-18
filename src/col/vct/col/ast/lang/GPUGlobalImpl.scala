package vct.col.ast.lang

import vct.col.ast.GPUGlobal
import vct.col.print.{Ctx, Doc, Text}

trait GPUGlobalImpl[G] {
  this: GPUGlobal[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("__vercors_global_memory__")
}
