package vct.col.ast.temporaryimplpackage.expr.context

import vct.col.ast.{Result, Type}

trait ResultImpl[G] { this: Result[G] =>
  override def t: Type[G] = applicable.decl.returnType
}