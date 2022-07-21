package vct.col.ast.temporaryimplpackage.expr.op.num

import vct.col.ast.{Div, TFloat, TRational, Type}
import vct.col.coerce.CoercionUtils

trait DivImpl[G] { this: Div[G] =>

  def isFloatOp: Boolean =
    CoercionUtils.getCoercion(left.t, TFloat()).isDefined &&
      CoercionUtils.getCoercion(right.t, TFloat()).isDefined

  override def t: Type[G] = {
    if (isFloatOp) TFloat()
    else TRational()
  }
}