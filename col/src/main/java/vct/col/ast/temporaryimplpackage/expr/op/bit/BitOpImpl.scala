package vct.col.ast.temporaryimplpackage.expr.op.bit

import vct.col.ast.{BitOp, TBool, TInt, Type}
import vct.col.coerce.Coercion

trait BitOpImpl { this: BitOp =>
  def isBoolOp: Boolean = Coercion.getCoercion(left.t, TBool()).isDefined
  override def t: Type = if(isBoolOp) TBool() else TInt()
}