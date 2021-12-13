package vct.col.ast.temporaryimplpackage.expr.literal.constant

import vct.col.ast.{OptNone, TAny, TOption, Type}

trait OptNoneImpl[G] { this: OptNone[G] =>
  override def t: Type[G] = TOption(TAny())
}