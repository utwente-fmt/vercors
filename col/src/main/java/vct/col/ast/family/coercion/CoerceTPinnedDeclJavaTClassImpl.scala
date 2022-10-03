package vct.col.ast.family.coercion

import vct.col.ast.{CoerceTPinnedDeclJavaTClass, JavaTClass, TPinnedDecl, Type}

trait CoerceTPinnedDeclJavaTClassImpl[G] { this: CoerceTPinnedDeclJavaTClass[G] =>
  override def target: Type[G] = cls
}
