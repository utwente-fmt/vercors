package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceJavaTClassTPinnedDecl, JavaTClass, TPinnedDecl, Type}

trait CoerceJavaTClassTPinnedDeclImpl[G] { this: CoerceJavaTClassTPinnedDecl[G] =>
  def target(): Type[G] = cls match {
    case JavaTClass(_, typeArgs) => TPinnedDecl(pin, typeArgs)
  }
}
