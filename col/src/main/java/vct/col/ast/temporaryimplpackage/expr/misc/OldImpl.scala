package vct.col.ast.temporaryimplpackage.expr.misc

import vct.col.ast.{Old, Type}

trait OldImpl[G] { this: Old[G] =>
  override def t: Type[G] = expr.t
}