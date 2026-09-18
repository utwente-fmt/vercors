package vct.col.ast.expr.op.either

import vct.col.ast.{IsRight, TBool, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.IsRightOps

trait IsRightImpl[G] extends IsRightOps[G] {
  this: IsRight[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = {
    ctx.syntax match {
      case Ctx.Isar => Text("¬ isl") <+> assoc(either)
      case _ => assoc(either) <> ".isRight"
    }

  }
}
