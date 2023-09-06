package vct.col.ast.expr.op.num

import vct.col.ast.CMod
import vct.col.print.{Ctx, Doc, Precedence}

trait CModImpl[G] { this: CMod[G] =>
  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "%", right)
}