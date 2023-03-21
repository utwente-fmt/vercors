package vct.col.ast.expr.lock

import vct.col.ast.{Held, TBool, Type}

trait HeldImpl[G] { this: Held[G] =>
  override def t: Type[G] = TBool()
}