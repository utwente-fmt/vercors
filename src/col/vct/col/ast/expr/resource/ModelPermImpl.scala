package vct.col.ast.expr.resource

import vct.col.ast.{ModelPerm, TResource, Type}

trait ModelPermImpl[G] { this: ModelPerm[G] =>
  override def t: Type[G] = TResource()
}