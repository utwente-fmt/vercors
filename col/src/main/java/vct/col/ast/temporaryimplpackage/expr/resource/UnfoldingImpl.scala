package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{Type, Unfolding}

trait UnfoldingImpl[G] { this: Unfolding[G] =>
  override def t: Type[G] = body.t
}