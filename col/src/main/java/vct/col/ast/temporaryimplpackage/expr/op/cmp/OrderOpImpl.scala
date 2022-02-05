package vct.col.ast.temporaryimplpackage.expr.op.cmp

import vct.col.ast.{OrderOp, TInt}
import vct.col.coerce.CoercionUtils

trait OrderOpImpl[G] { this: OrderOp[G] =>
  def isSetOp: Boolean = CoercionUtils.getAnySetCoercion(left.t).isDefined

  def isBagOp: Boolean = CoercionUtils.getAnyBagCoercion(left.t).isDefined

  def isIntOp: Boolean =
    CoercionUtils.getCoercion(left.t, TInt()).isDefined &&
      CoercionUtils.getCoercion(right.t, TInt()).isDefined
}