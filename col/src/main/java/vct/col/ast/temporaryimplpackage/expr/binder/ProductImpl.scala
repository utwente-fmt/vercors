package vct.col.ast.temporaryimplpackage.expr.binder

import vct.col.ast.{Product, TInt, Type}

trait ProductImpl[G] { this: Product[G] =>
  override def t: Type[G] = TInt()
}
