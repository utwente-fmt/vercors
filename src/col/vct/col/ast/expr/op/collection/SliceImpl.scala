package vct.col.ast.expr.op.collection

import vct.col.ast.{Slice, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.SliceOps

trait SliceImpl[G] extends SliceOps[G] {
  this: Slice[G] =>
  override def t: Type[G] = xs.t

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = {
    ctx.syntax match {
      case Ctx.Isar => Text("nths") <+> assoc(xs) <+> "{" <> from <> ".." <> to <> "}"
      case _ => assoc(xs) <> "[" <> from <> ".." <> to <> "]"
    }
  }
}
