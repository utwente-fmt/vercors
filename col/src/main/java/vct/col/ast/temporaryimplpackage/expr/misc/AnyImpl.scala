package vct.col.ast.temporaryimplpackage.expr.misc

import vct.col.ast.{Any, TInt, Type}

trait AnyImpl { this: Any =>
  override def t: Type = TInt()
}