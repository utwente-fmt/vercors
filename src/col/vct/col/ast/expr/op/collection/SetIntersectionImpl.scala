package vct.col.ast.expr.op.collection

import vct.col.ast.{SetIntersection, TSet}
import vct.col.print.{Ctx, Doc, Group, Precedence}
import vct.col.typerules.Types
import vct.col.ast.ops.SetIntersectionOps

trait SetIntersectionImpl[G] extends SetIntersectionOps[G] {
  this: SetIntersection[G] =>
  def xsType: TSet[G] = xs.t.asSet.get
  def ysType: TSet[G] = ys.t.asSet.get
  override lazy val t: TSet[G] = TSet(
    Types.leastCommonSuperType(xsType.element, ysType.element)
  )

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(xs) <> ".intersect(" <> Doc.arg(ys) <> ")")
}
