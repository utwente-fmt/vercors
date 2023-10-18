package vct.col.ast.statement.terminal

import vct.col.ast.Exhale
import vct.col.print.{Ctx, Doc, Show, Text}

trait ExhaleImpl[G] {
  this: Exhale[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc = Text("exhale") <+> res <> ";"

  def layoutSilver(implicit ctx: Ctx): Doc = Text("exhale") <+> res

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => layoutSilver
      case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
    }
}
