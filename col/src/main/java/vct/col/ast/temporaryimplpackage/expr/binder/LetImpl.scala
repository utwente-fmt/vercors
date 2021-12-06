package vct.col.ast.temporaryimplpackage.expr.binder

import vct.col.ast.{Let, Type, Variable}

trait LetImpl { this: Let =>
  override def t: Type = main.t
  override def bindings: Seq[Variable] = Seq(binding)
}