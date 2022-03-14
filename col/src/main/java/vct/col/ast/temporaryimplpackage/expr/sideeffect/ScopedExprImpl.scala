package vct.col.ast.temporaryimplpackage.expr.sideeffect

import vct.col.ast.{ScopedExpr, Type}

trait ScopedExprImpl[G] { this: ScopedExpr[G] =>
  override def t: Type[G] = body.t
}
