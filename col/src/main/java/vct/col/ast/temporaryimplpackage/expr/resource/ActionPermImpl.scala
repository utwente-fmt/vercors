package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{ActionPerm, TResource, Type}

trait ActionPermImpl[G] { this: ActionPerm[G] =>
  override def t: Type[G] = TResource()
}
