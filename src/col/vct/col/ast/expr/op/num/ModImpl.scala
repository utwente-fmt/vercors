package vct.col.ast.expr.op.num

import vct.col.ast.Mod
import vct.col.print.{Ctx, Doc, Precedence}

trait ModImpl[G] { this: Mod[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "%", right)
}