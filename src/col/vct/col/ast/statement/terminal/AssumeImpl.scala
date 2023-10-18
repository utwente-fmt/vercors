package vct.col.ast.statement.terminal

import vct.col.ast.Assume
import vct.col.print.{Ctx, Doc, Show, Text}

trait AssumeImpl[G] {
  this: Assume[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc = Text("assume") <+> assn <> ";"

  def layoutSilver(implicit ctx: Ctx): Doc = Text("assume") <+> assn

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => layoutSilver
      case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
    }
}
