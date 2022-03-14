package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{StringConcat, TString, Type}

trait StringConcatImpl[G] { this: StringConcat[G] =>
  override def t: Type[G] = TString()
}
