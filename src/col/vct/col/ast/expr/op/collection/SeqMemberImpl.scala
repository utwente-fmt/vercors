package vct.col.ast.expr.op.collection

import vct.col.ast.{SeqMember, TBool, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.SeqMemberOps

trait SeqMemberImpl[G] extends SeqMemberOps[G] {
  this: SeqMember[G] =>
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
