package vct.col.ast.expr.misc

import vct.col.ast.{StringValue, TString, Type}

trait StringLiteralImpl[G] {
  this: StringValue[G] =>
  override def t: Type[G] = TString()
}
