package vct.col.ast.statement.terminal

import vct.col.ast.{Assume, Expr}
import vct.col.print.{Ctx, Doc, Show, Text}
import vct.col.ast.ops.AssumeOps

trait AssumeImpl[G] extends AssumeOps[G] { this: Assume[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Text("assume") <+> assn <> ";"

  def layoutSilver(implicit ctx: Ctx): Doc =
    Text("assume") <+> assn

  override def layout(implicit ctx: Ctx): Doc = ctx.syntax match {
    case Ctx.Silver => layoutSilver
    case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
  }

  override def expr: Expr[G] = this.assn
}