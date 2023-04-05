package vct.col.ast.expr.op.collection

import vct.col.ast.{SeqMember, TBool, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait SeqMemberImpl[G] { this: SeqMember[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.RELATIONAL
  override def layout(implicit ctx: Ctx): Doc = lassoc(x, "\\in", xs)
}