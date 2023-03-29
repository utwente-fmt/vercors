package vct.col.ast.expr.misc

import vct.col.ast.{Old, Type}

trait OldImpl[G] { this: Old[G] =>
  override def t: Type[G] = expr.t
}