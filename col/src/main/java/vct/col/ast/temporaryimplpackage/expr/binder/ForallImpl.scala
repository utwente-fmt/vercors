package vct.col.ast.temporaryimplpackage.expr.binder

import vct.col.ast.{Forall, TBool, Type}

trait ForallImpl { this: Forall =>
  override def t: Type = TBool()
}
