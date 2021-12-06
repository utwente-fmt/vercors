package vct.col.ast.temporaryimplpackage.expr.misc

import vct.col.ast.{Old, Type}

trait OldImpl { this: Old =>
  override def t: Type = expr.t
}