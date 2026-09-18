package vct.col.ast.lang.llvm

import vct.col.ast.F32
import vct.col.ast.ops.F32Ops
import vct.col.print._

trait F32Impl[G] extends F32Ops[G] {
  this: F32[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("f32")
}
