package vct.col.ast.temporaryimplpackage.expr.literal.constant

import vct.col.ast.{OptNone, TAny, TOption, Type}

trait OptNoneImpl { this: OptNone =>
  override def t: Type = TOption(TAny())
}