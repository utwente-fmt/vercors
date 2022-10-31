package vct.col.ast.expr.op.bool

import vct.col.ast.{Not, TBool, Type}

trait NotImpl[G] { this: Not[G] =>
  override def t: Type[G] = TBool()
}