package vct.col.ast.temporaryimplpackage.expr.lock

import vct.col.ast.{IdleToken, TResource, Type}

trait IdleTokenImpl[G] { this: IdleToken[G] =>
  override def t: Type[G] = TResource()
}