package vct.col.ast.expr.op.either

import vct.col.ast.{GetLeft, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.GetLeftOps

trait GetLeftImpl[G] extends GetLeftOps[G] { this: GetLeft[G] =>
  override def t: Type[G] = eitherType.left

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = assoc(either) <> ".left"
}