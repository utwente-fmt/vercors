package vct.col.ast.statement.terminal

import vct.col.ast.WandApply
import vct.col.print.{Ctx, Doc, Show, Text}

trait WandApplyImpl[G] {
  this: WandApply[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc = Text("apply") <+> res <> ";"

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => Text("apply") <+> res
      case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
    }
}
