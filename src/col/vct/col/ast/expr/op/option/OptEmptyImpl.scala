package vct.col.ast.expr.op.option

import vct.col.ast.{OptEmpty, TBool, Type}

trait OptEmptyImpl[G] { this: OptEmpty[G] =>
  override def t: Type[G] = TBool()
}