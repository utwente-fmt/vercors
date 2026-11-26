package vct.col.ast.expr.op.collection

import vct.col.ast.{SetMember, TBool, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.SetMemberOps

trait SetMemberImpl[G] extends SetMemberOps[G] {
  this: SetMember[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc =
    lassoc(
      x,
      ctx.syntax match {
        case Ctx.Silver => "in"
        case Ctx.Isar => "|∈|"
        case _ => "\\in"
      },
      xs,
    )
}
