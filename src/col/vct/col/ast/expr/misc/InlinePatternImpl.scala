package vct.col.ast.expr.misc

import vct.col.ast.{InlinePattern, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait InlinePatternImpl[G] { this: InlinePattern[G] =>
  override def t: Type[G] = inner.t

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = (parent, group) match {
    case (0, 0) => Text("{:") <+> inner <+> ":}"
    case (parent, 0) => Text("{:") <> "<".repeat(parent) <> ":" <+> inner <+> ":}"
    case (parent, group) => Text("{:") <> "<".repeat(parent) <> group.toString <> ":" <+> inner <+> ":}"
  }
}