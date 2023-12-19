package vct.col.ast.expr.sideeffect

import vct.col.ast.{IndeterminateInteger, TInt, Type}

trait IndeterminateIntegerImpl[G] { this: IndeterminateInteger[G] =>
  override def t: Type[G] = TInt()
}
