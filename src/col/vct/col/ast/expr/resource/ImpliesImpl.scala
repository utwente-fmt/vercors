package vct.col.ast.expr.resource

import vct.col.ast.{Implies, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait ImpliesImpl[G] { this: Implies[G] =>
  override def t: Type[G] = right.t

  override def precedence: Int = Precedence.IMPLIES
  override def layout(implicit ctx: Ctx): Doc = rassoc(left, "==>", right)
}