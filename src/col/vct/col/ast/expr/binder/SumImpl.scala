package vct.col.ast.expr.binder

import vct.col.ast.{Sum, TInt, Type}

trait SumImpl[G] { this: Sum[G] =>
  override def t: Type[G] = TInt()
}
