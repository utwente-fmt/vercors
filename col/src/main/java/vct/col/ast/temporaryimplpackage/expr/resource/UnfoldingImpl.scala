package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{Type, Unfolding}

trait UnfoldingImpl { this: Unfolding =>
  override def t: Type = body.t
}