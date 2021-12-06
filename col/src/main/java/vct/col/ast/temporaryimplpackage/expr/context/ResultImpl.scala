package vct.col.ast.temporaryimplpackage.expr.context

import vct.col.ast.{Result, Type}

trait ResultImpl { this: Result =>
  override def t: Type = applicable.decl.returnType
}