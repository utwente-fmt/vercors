package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{ModelPerm, TResource, Type}

trait ModelPermImpl { this: ModelPerm =>
  override def t: Type = TResource()
}