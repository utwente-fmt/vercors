package vct.col.ast.expr.misc

import vct.col.ast.{CharValue, TChar, Type}

trait CharLiteralImpl[G] { this: CharValue[G] =>
  def t: Type[G] = TChar()
}
