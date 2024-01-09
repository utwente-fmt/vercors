package vct.col.ast.expr.op.collection

import vct.col.ast.{BagMemberCount, TInt, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence}
import vct.col.ast.ops.BagMemberCountOps

trait BagMemberCountImpl[G] extends BagMemberCountOps[G] { this: BagMemberCount[G] =>
  override def t: Type[G] = TInt()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(xs) <> ".count(" <> Doc.arg(x) <> ")")
}