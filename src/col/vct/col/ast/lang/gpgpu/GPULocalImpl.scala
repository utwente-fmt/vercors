package vct.col.ast.lang.gpgpu

import vct.col.ast.GPULocal
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.GPULocalOps

trait GPULocalImpl[G] extends GPULocalOps[G] {this: GPULocal[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("__vercors_local_memory__")
}