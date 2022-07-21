package vct.col.ast.temporaryimplpackage.expr.op.num

import vct.col.ast.{FloorDiv, TFloat, TInt, Type}
import vct.col.coerce.CoercionUtils

trait FloorDivImpl[G] { this: FloorDiv[G] =>
  def isIntOp: Boolean =
    CoercionUtils.getCoercion(left.t, TInt()).isDefined &&
      CoercionUtils.getCoercion(right.t, TInt()).isDefined

  override def t: Type[G] = {
    if (isIntOp) TInt()
    else TFloat()
  }
}