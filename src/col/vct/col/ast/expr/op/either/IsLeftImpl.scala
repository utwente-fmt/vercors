package vct.col.ast.expr.op.either

import vct.col.ast.{IsLeft, TBool, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.IsLeftOps

trait IsLeftImpl[G] extends IsLeftOps[G] {
  this: IsLeft[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = {
    ctx.syntax match {
      case Ctx.Isar => Text("isl") <+> assoc(either)
      case _ => assoc(either) <> ".isLeft"
    }

  }
}
