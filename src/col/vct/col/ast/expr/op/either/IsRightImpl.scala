package vct.col.ast.expr.op.either

import vct.col.ast.{IsRight, TBool, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait IsRightImpl[G] { this: IsRight[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = assoc(either) <> ".isRight"
}