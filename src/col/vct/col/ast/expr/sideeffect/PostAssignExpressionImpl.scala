package vct.col.ast.expr.sideeffect

import vct.col.ast.{PostAssignExpression, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait PostAssignExpressionImpl[G] {
  this: PostAssignExpression[G] =>
  override def t: Type[G] = target.t

  override def precedence: Int = Precedence.ASSIGN
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      nassoc(target) <+>
        (if (ctx.syntax == Ctx.Silver)
           ":="
         else
           "=") <>> assoc(value)
    )
}
