package vct.col.ast.expr.op.collection

import vct.col.ast.{BagLargestCommon, TBag}
import vct.col.print._
import vct.col.typerules.Types
import vct.col.ast.ops.BagLargestCommonOps

trait BagLargestCommonImpl[G] extends BagLargestCommonOps[G] {
  this: BagLargestCommon[G] =>
  def xsType: TBag[G] = xs.t.asBag.get
  def ysType: TBag[G] = ys.t.asBag.get
  override lazy val t: TBag[G] = TBag(
    Types.leastCommonSuperType(xsType.element, ysType.element)
  )

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(xs) <> ".intersect(" <> Doc.arg(ys) <> ")")
}
