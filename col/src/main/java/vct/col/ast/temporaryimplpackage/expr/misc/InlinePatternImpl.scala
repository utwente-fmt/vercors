package vct.col.ast.temporaryimplpackage.expr.misc

import vct.col.ast.{InlinePattern, Type}

trait InlinePatternImpl { this: InlinePattern =>
  override def t: Type = inner.t
}