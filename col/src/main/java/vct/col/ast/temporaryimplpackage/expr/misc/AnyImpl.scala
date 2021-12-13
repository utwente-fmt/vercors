package vct.col.ast.temporaryimplpackage.expr.misc

import vct.col.ast.{Any, TInt, Type}

trait AnyImpl[G] { this: Any[G] =>
  override def t: Type[G] = TInt()
}