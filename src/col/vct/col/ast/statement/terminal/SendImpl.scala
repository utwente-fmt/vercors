package vct.col.ast.statement.terminal

import vct.col.ast.{Expr, Send}
import vct.col.print.{Ctx, Doc, Group, Show, Text}
import vct.col.ast.ops.SendOps

trait SendImpl[G] extends SendOps[G] { this: Send[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Group(Text("send") <+> ctx.name(decl) <> "," <+> delta.toString <> ":" <>> res <> ";")

  override def layout(implicit ctx: Ctx): Doc =
    Doc.inlineSpec(Show.lazily(layoutSpec(_)))

  override def expr: Expr[G] = this.res
}