package vct.col.ast.statement.terminal

import vct.col.ast.{Expr, Inhale}
import vct.col.print.{Ctx, Doc, Nest, Show, Text}
import vct.col.ast.ops.InhaleOps

trait InhaleImpl[G] extends InhaleOps[G] {
  this: Inhale[G] =>
  def layoutSpec(implicit ctx: Ctx): Doc =
    Text("inhale") <+> Nest(res.show) <> ";"

  def layoutSilver(implicit ctx: Ctx): Doc = Text("inhale") <+> Nest(res.show)

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => layoutSilver
      case _ => Doc.inlineSpec(Show.lazily(layoutSpec(_)))
    }

  override def expr: Expr[G] = this.res
}
