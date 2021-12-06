package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{APerm, TResource, Type}

trait APermImpl { this: APerm =>
  override def t: Type = TResource()
}