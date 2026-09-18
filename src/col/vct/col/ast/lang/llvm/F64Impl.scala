package vct.col.ast.lang.llvm

import vct.col.ast.F64
import vct.col.ast.ops.F64Ops
import vct.col.print._

trait F64Impl[G] extends F64Ops[G] {
  this: F64[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("f64")
}
