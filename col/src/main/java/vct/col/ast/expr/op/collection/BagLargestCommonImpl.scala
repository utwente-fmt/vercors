package vct.col.ast.expr.op.collection

import vct.col.ast.{BagLargestCommon, TBag}
import vct.col.typerules.Types

trait BagLargestCommonImpl[G] { this: BagLargestCommon[G] =>
  def xsType: TBag[G] = xs.t.asBag.get
  def ysType: TBag[G] = ys.t.asBag.get
  override lazy val t: TBag[G] = TBag(Types.leastCommonSuperType(xsType.element, ysType.element))
}
