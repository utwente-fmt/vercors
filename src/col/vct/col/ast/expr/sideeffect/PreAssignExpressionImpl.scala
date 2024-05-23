package vct.col.ast.expr.sideeffect

import vct.col.ast.{PreAssignExpression, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence}
import vct.col.ast.ops.PreAssignExpressionOps

trait PreAssignExpressionImpl[G] extends PreAssignExpressionOps[G] { this: PreAssignExpression[G] =>
  override def t: Type[G] = value.t

  override def precedence: Int = Precedence.ASSIGN
  override def layout(implicit ctx: Ctx): Doc =
    Group(nassoc(target) <+> (if(ctx.syntax == Ctx.Silver) ":=" else "=") <>> assoc(value))
}