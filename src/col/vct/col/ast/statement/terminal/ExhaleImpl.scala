package vct.col.ast.statement.terminal

import vct.col.ast.{Exhale, Expr}
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.ExhaleOps

trait ExhaleImpl[G] extends ExhaleOps[G] {
  this: Exhale[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc = Text("exhale") <+> res <> ";"

  def layoutSilver(implicit ctx: Ctx): Doc = Text("exhale") <+> res

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => layoutSilver
      case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
    }

  override def expr: Expr[G] = this.res
}
