package vct.col.ast.temporaryimplpackage.expr.op.bit

import vct.col.ast.{BitOp, TBool, TInt, Type}
import vct.col.coerce.Coercion

trait BitOpImpl[G] { this: BitOp[G] =>
  def isBoolOp: Boolean = Coercion.getCoercion(left.t, TBool()).isDefined
  override def t: Type[G] = if(isBoolOp) TBool() else TInt()
}