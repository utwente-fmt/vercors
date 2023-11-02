package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibPow, TBool, Type}
import vct.col.print._

trait SmtlibPowImpl[G] { this: SmtlibPow[G] =>
  override def precedence: Int = Precedence.PVL_POW

  override def t: Type[G] = left.t
  override def layout(implicit ctx: Ctx): Doc = rassoc(left, "^", right)
}
