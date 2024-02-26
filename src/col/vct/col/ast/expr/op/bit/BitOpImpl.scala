package vct.col.ast.expr.op.bit

import vct.col.ast.{BitOp, TBool, TCInt, TInt, Type}
import vct.col.typerules.CoercionUtils

trait BitOpImpl[G] { this: BitOp[G] =>
  override def t: Type[G] = if(isBoolOp) TBool() else if(isCIntOp) TCInt() else TInt()
}