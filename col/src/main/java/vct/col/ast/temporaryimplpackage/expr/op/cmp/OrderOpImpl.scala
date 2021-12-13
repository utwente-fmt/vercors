package vct.col.ast.temporaryimplpackage.expr.op.cmp

import vct.col.ast.{OrderOp, TInt}
import vct.col.coerce.Coercion

trait OrderOpImpl[G] { this: OrderOp[G] =>
  def isSetOp: Boolean = Coercion.getAnySetCoercion(left.t).isDefined

  def isBagOp: Boolean = Coercion.getAnyBagCoercion(left.t).isDefined

  def isIntOp: Boolean =
    Coercion.getCoercion(left.t, TInt()).isDefined &&
      Coercion.getCoercion(right.t, TInt()).isDefined
}