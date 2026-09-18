package vct.col.ast.lang.llvm

import vct.col.ast.F80
import vct.col.ast.ops.F80Ops
import vct.col.print._

trait F80Impl[G] extends F80Ops[G] {
  this: F80[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("f80")
}
