package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{StringLiteral, TString, Type}

trait StringLiteralImpl[G] { this: StringLiteral[G] =>
  override def t: Type[G] = TString()
}
