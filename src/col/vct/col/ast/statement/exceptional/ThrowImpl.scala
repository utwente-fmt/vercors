package vct.col.ast.statement.exceptional

import vct.col.ast.{Expr, Throw}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.ThrowOps

trait ThrowImpl[G] extends ThrowOps[G] {
  this: Throw[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("throw") <+> obj <> ";"

  override def expr: Expr[G] = this.obj
}
