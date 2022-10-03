package vct.col.ast.expr.op.cmp

import vct.col.ast.{MapEq, TBool, Type}

trait MapEqImpl[G] { this: MapEq[G] =>
  override def t: Type[G] = TBool()
}
