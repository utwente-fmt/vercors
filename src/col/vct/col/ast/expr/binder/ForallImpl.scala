package vct.col.ast.expr.binder

import vct.col.ast.{Forall, TBool, Type}

trait ForallImpl[G] { this: Forall[G] =>
  override def t: Type[G] = TBool()
}
