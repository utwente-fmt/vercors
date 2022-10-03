package vct.col.ast.expr.misc

import vct.col.ast.{Any, TInt, Type}

trait AnyImpl[G] { this: Any[G] =>
  override def t: Type[G] = TInt()
}