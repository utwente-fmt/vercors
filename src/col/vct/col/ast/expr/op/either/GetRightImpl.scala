package vct.col.ast.expr.op.either

import vct.col.ast.{GetRight, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait GetRightImpl[G] { this: GetRight[G] =>
  override def t: Type[G] = eitherType.right

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = assoc(either) <> ".right"
}