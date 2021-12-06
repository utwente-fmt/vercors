package vct.col.ast.temporaryimplpackage.expr.binder

import vct.col.ast.{Product, TInt, Type}

trait ProductImpl { this: Product =>
  override def t: Type = TInt()
}
