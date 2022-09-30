package vct.col.ast.expr.op.collection

import vct.col.ast.{SetIntersection, TSet}
import vct.col.typerules.Types

trait SetIntersectionImpl[G] { this: SetIntersection[G] =>
  def xsType: TSet[G] = xs.t.asSet.get
  def ysType: TSet[G] = ys.t.asSet.get
  override def t: TSet[G] = TSet(Types.leastCommonSuperType(xsType.element, ysType.element))
}
