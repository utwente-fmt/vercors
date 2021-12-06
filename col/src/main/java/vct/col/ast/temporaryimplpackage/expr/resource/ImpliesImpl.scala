package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{Implies, Type}

trait ImpliesImpl { this: Implies =>
  override def t: Type = right.t
}