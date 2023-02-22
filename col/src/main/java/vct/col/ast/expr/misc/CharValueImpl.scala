package vct.col.ast.expr.misc

import vct.col.ast.{CharValue, TChar, Type}

trait CharValueImpl[G] { this: CharValue[G] =>
  def t: Type[G] = TChar()
}
