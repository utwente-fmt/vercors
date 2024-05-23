package vct.col.ast.lang.gpgpu

import vct.col.ast.GPUGlobal
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.GPUGlobalOps

trait GPUGlobalImpl[G] extends GPUGlobalOps[G] {
  this: GPUGlobal[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("__vercors_global_memory__")
}
