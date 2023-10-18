package vct.col.ast.statement.terminal

import vct.col.ast.Inhale
import vct.col.print.{Ctx, Doc, Show, Text}

trait InhaleImpl[G] {
  this: Inhale[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc = Text("inhale") <+> res <> ";"

  def layoutSilver(implicit ctx: Ctx): Doc = Text("inhale") <+> res

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => layoutSilver
      case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
    }
}
