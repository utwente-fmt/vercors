package vct.col.ast.statement.terminal

import vct.col.ast.{Expr, Join}
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.JoinOps

trait JoinImpl[G] extends JoinOps[G] { this: Join[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Text("join") <+> obj <> ";"

  override def layout(implicit ctx: Ctx): Doc = Doc.inlineSpec(Show.lazily(layoutSpec(_)))

  override def expr: Expr[G] = this.obj
}