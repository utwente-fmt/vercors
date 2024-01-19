package vct.col.ast.expr.resource

import vct.col.ast.{Implies, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.ImpliesOps

trait ImpliesImpl[G] extends ImpliesOps[G] { this: Implies[G] =>
  override def t: Type[G] = right.t

  override def precedence: Int = Precedence.IMPLIES
  override def layout(implicit ctx: Ctx): Doc = rassoc(left, "==>", right)
}