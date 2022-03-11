package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceTPinnedDeclJavaTClass, JavaTClass, TPinnedDecl, Type}

trait CoerceTPinnedDeclJavaTClassImpl[G] { this: CoerceTPinnedDeclJavaTClass[G] =>
  override def target: Type[G] = cls
}
