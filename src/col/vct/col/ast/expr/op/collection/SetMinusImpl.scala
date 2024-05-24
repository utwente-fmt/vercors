package vct.col.ast.expr.op.collection

import vct.col.ast.{SetMinus, TSet}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.typerules.Types
import vct.col.ast.ops.SetMinusOps

trait SetMinusImpl[G] extends SetMinusOps[G] { this: SetMinus[G] =>
  def xsType: TSet[G] = xs.t.asSet.get
  def ysType: TSet[G] = ys.t.asSet.get
  override lazy val t: TSet[G] = TSet(Types.leastCommonSuperType(xsType.element, ysType.element))

  override def precedence: Int = Precedence.ADDITIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(xs, "-", ys)
}
