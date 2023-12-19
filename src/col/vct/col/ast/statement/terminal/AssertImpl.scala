package vct.col.ast.statement.terminal

import vct.col.ast.Assert
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.AssertOps

trait AssertImpl[G] extends AssertOps[G] { this: Assert[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Text("assert") <+> res <> ";"

  def layoutSilver(implicit ctx: Ctx): Doc =
    Text("assert") <+> res

  override def layout(implicit ctx: Ctx): Doc = ctx.syntax match {
    case Ctx.Silver => layoutSilver
    case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
  }
}