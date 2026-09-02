package vct.col.ast.expr.op.either

import vct.col.ast.{GetRight, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.GetRightOps

trait GetRightImpl[G] extends GetRightOps[G] {
  this: GetRight[G] =>
  override def t: Type[G] = eitherType.right

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Isar => Group(Text("projr") <+> assoc(either))
      case _ => assoc(either) <> ".right"
    }
}
