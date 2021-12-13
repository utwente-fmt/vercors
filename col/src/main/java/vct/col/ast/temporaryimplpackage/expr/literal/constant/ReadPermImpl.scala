package vct.col.ast.temporaryimplpackage.expr.literal.constant

import vct.col.ast.{ReadPerm, TFraction, Type}

trait ReadPermImpl[G] { this: ReadPerm[G] =>
  override def t: Type[G] = TFraction()
}