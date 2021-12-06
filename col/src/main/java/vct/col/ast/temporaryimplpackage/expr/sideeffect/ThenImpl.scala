package vct.col.ast.temporaryimplpackage.expr.sideeffect

import vct.col.ast.{Then, Type}

trait ThenImpl { this: Then =>
  override def t: Type = value.t
}