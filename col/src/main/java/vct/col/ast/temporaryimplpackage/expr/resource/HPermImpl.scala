package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{HPerm, TResource, Type}

trait HPermImpl[G] { this: HPerm[G] =>
  override def t: Type[G] = TResource()
}