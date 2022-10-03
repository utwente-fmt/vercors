package vct.col.ast.family.coercion

import vct.col.ast.{CoerceTClassTPinnedDecl, TClass, TPinnedDecl, Type}

trait CoerceTClassTPinnedDeclImpl[G] { this: CoerceTClassTPinnedDecl[G] =>
  override def target: Type[G] = TPinnedDecl(pin, Nil)
}
