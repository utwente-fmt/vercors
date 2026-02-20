package vct.col.ast.expr.op.num

import vct.col.ast.ops.UncheckedMathOps
import vct.col.ast.{UncheckedMath, Type}
import vct.col.print.{Ctx, Doc, Text}

trait UncheckedMathImpl[G] extends UncheckedMathOps[G] {
  this: UncheckedMath[G] =>
  override def t: Type[G] = inner.t

  override def layout(implicit ctx: Ctx): Doc =
    Text("\\unchecked(") <> inner <> ")"
}
