package vct.col.ast.expr.misc

import vct.col.ast.{CharLiteral, TChar, Type}

trait CharLiteralImpl[G] { this: CharLiteral[G] =>
  def t: Type[G] = TChar()
}
