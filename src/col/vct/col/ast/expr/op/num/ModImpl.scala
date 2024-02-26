package vct.col.ast.expr.op.num

import vct.col.ast.{Mod, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.ModOps

trait ModImpl[G] extends ModOps[G] { this: Mod[G] =>
  override def t: Type[G] = TInt()
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "%", right)
}