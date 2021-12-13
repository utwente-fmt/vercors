package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{Perm, TResource, Type}

trait PermImpl[G] { this: Perm[G] =>
  override def t: Type[G] = TResource()
}