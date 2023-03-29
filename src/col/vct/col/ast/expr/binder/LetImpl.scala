package vct.col.ast.expr.binder

import vct.col.ast.{Let, Type, Variable}

trait LetImpl[G] { this: Let[G] =>
  override def t: Type[G] = main.t
  override def bindings: Seq[Variable[G]] = Seq(binding)
}