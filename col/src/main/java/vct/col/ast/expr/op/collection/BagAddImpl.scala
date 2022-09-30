package vct.col.ast.expr.op.collection

import vct.col.ast.{BagAdd, TBag}
import vct.col.typerules.Types

trait BagAddImpl[G] { this: BagAdd[G] =>
  def xsType: TBag[G] = xs.t.asBag.get
  def ysType: TBag[G] = ys.t.asBag.get
  override def t: TBag[G] = TBag(Types.leastCommonSuperType(xsType.element, ysType.element))
}
