package vct.col.ast.expr.misc

import vct.col.ast.{StringLiteral, TString, Type}

trait StringLiteralImpl[G] {
  this: StringLiteral[G] =>
  override def t: Type[G] = TString()
}
