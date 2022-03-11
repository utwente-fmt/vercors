package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceJavaTClassTPinnedDecl, TPinnedDecl, Type}

trait CoerceJavaTClassTPinnedDeclImpl[G] { this: CoerceJavaTClassTPinnedDecl[G] =>
  override def target: Type[G] = TPinnedDecl(pin)
}
