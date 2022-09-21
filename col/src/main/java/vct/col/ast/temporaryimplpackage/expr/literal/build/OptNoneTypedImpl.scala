package vct.col.ast.temporaryimplpackage.expr.literal.build

import vct.col.ast.{OptNoneTyped, TOption, Type}

trait OptNoneTypedImpl[G] { this: OptNoneTyped[G] =>
  override def t: Type[G] = TOption(element)
}
