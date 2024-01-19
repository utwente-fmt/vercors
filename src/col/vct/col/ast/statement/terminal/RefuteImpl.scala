package vct.col.ast.statement.terminal

import vct.col.ast.Refute
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.RefuteOps

trait RefuteImpl[G] extends RefuteOps[G] { this: Refute[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Text("refute") <+> assn <> ";"

  def layoutSilver(implicit ctx: Ctx): Doc =
    Text("refute") <+> assn

  override def layout(implicit ctx: Ctx): Doc = ctx.syntax match {
    case Ctx.Silver => layoutSilver
    case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
  }
}