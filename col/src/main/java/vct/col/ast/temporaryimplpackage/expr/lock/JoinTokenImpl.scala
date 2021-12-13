package vct.col.ast.temporaryimplpackage.expr.lock

import vct.col.ast.{JoinToken, TResource, Type}

trait JoinTokenImpl[G] { this: JoinToken[G] =>
  override def t: Type[G] = TResource()
}