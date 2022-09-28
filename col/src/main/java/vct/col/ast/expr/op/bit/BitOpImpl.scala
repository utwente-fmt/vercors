package vct.col.ast.expr.op.bit

import vct.col.ast.{BitOp, TBool, TInt, Type}
import vct.col.typerules.CoercionUtils

trait BitOpImpl[G] { this: BitOp[G] =>
  def isBoolOp: Boolean = CoercionUtils.getCoercion(left.t, TBool()).isDefined
  override def t: Type[G] = if(isBoolOp) TBool() else TInt()
}