package vct.col.ast.temporaryimplpackage.expr.literal.constant

import vct.col.ast.{ReadPerm, TFraction, Type}

trait ReadPermImpl { this: ReadPerm =>
  override def t: Type = TFraction()
}