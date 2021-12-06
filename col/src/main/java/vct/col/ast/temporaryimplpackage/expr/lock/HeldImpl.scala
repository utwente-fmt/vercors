package vct.col.ast.temporaryimplpackage.expr.lock

import vct.col.ast.{Held, TBool, Type}

trait HeldImpl { this: Held =>
  override def t: Type = TBool()
}