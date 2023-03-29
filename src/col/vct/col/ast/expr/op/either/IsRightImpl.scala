package vct.col.ast.expr.op.either

import vct.col.ast.{IsRight, TBool, Type}

trait IsRightImpl[G] { this: IsRight[G] =>
  override def t: Type[G] = TBool()
}