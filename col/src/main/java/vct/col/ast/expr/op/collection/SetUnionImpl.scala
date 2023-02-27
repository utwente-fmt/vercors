package vct.col.ast.expr.op.collection

import vct.col.ast.{SetUnion, TSet}
import vct.col.typerules.Types

trait SetUnionImpl[G] { this: SetUnion[G] =>
  def xsType: TSet[G] = xs.t.asSet.get
  def ysType: TSet[G] = ys.t.asSet.get
  override lazy val t: TSet[G] = TSet(Types.leastCommonSuperType(xsType.element, ysType.element))
}
