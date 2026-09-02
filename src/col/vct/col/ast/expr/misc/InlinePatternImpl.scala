package vct.col.ast.expr.misc

import vct.col.ast.{InlinePattern, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.InlinePatternOps

trait InlinePatternImpl[G] extends InlinePatternOps[G] {
  this: InlinePattern[G] =>
  override def t: Type[G] = inner.t

  override def precedence: Int = Precedence.ATOMIC
  def layoutDefault(implicit ctx: Ctx) =
    (parent, group) match {
      case (0, 0) => Text("{:") <+> inner <+> ":}"
      case (parent, 0) =>
        Text("{:") <> "<".repeat(parent) <> ":" <+> inner <+> ":}"
      case (parent, group) =>
        Text("{:") <> "<".repeat(parent) <> group.toString <> ":" <+> inner <+>
          ":}"
    }

  // layout
  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Isar =>
        if (ctx.translateTriggers)
          layoutDefault
        else
          inner.show
      case _ => layoutDefault
    }
}
