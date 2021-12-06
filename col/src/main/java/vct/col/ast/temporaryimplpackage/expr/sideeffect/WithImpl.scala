package vct.col.ast.temporaryimplpackage.expr.sideeffect

import vct.col.ast.{Type, With}

trait WithImpl { this: With =>
  override def t: Type = value.t
}