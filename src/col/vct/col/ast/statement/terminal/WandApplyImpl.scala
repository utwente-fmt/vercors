package vct.col.ast.statement.terminal

import vct.col.ast.{Expr, WandApply}
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.WandApplyOps

trait WandApplyImpl[G] extends WandApplyOps[G] {
  this: WandApply[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc = Text("apply") <+> res <> ";"

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => Text("apply") <+> res
      case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
    }

  override def expr: Expr[G] = this.res
}
