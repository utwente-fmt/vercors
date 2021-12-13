package vct.col.ast.temporaryimplpackage.expr.misc

import vct.col.ast.{InlinePattern, Type}

trait InlinePatternImpl[G] { this: InlinePattern[G] =>
  override def t: Type[G] = inner.t
}