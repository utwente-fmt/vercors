package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{Perm, TResource, Type}

trait PermImpl { this: Perm =>
  override def t: Type = TResource()
}