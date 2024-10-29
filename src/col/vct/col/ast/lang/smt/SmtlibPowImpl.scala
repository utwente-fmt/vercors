package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibPow, TBool, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibPowOps

trait SmtlibPowImpl[G] extends SmtlibPowOps[G] {
  this: SmtlibPow[G] =>
  override def precedence: Int = Precedence.PVL_POW

  override lazy val t: Type[G] = getNumericType
  override def layout(implicit ctx: Ctx): Doc = rassoc(left, "^", right)
}
