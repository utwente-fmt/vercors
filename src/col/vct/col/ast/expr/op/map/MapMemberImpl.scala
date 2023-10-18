package vct.col.ast.expr.op.map

import vct.col.ast.{MapMember, TBool, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait MapMemberImpl[G] {
  this: MapMember[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc =
    lassoc(
      x,
      if (ctx.syntax == Ctx.Silver)
        "in"
      else
        "\\in",
      xs,
    )
}
