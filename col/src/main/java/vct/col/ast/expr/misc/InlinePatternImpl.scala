package vct.col.ast.expr.misc

import vct.col.ast.{InlinePattern, Type}

trait InlinePatternImpl[G] { this: InlinePattern[G] =>
  override def t: Type[G] = inner.t
}