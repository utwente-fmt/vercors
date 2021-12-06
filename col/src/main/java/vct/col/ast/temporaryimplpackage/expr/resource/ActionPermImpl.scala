package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{ActionPerm, TResource, Type}

trait ActionPermImpl { this: ActionPerm =>
  override def t: Type = TResource()
}
