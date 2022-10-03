package vct.col.ast.family.coercion

import vct.col.ast.{CoerceTPinnedDeclTClass, Type}

trait CoerceTPinnedDeclTClassImpl[G] { this: CoerceTPinnedDeclTClass[G] =>
  override def target: Type[G] = cls
}
