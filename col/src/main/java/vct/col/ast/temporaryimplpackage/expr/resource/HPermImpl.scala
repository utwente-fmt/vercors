package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{HPerm, TResource, Type}

trait HPermImpl { this: HPerm =>
  override def t: Type = TResource()
}