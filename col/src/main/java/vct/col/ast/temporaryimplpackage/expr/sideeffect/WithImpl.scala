package vct.col.ast.temporaryimplpackage.expr.sideeffect

import vct.col.ast.{Type, With}

trait WithImpl[G] { this: With[G] =>
  override def t: Type[G] = value.t
}