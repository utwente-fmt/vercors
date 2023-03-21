package vct.col.ast.expr.resource

import vct.col.ast.{Implies, Type}

trait ImpliesImpl[G] { this: Implies[G] =>
  override def t: Type[G] = right.t
}