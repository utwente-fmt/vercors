package vct.col.ast.expr.literal.constant

import vct.col.ast.{OptNone, TNothing, TOption, Type}

trait OptNoneImpl[G] { this: OptNone[G] =>
  override def t: Type[G] = TOption(TNothing())
}