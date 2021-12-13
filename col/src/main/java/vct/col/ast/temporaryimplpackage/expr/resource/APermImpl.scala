package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{APerm, TResource, Type}

trait APermImpl[G] { this: APerm[G] =>
  override def t: Type[G] = TResource()
}