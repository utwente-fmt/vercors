package vct.col.ast.expr.resource

import vct.col.ast.{Star, TResource, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait StarImpl[G] { this: Star[G] =>
  override def t: Type[G] = TResource()

  override def precedence: Int = Precedence.AND
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, if(ctx.syntax == Ctx.Silver) "&&" else "**", right)
}