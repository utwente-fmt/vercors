package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceTClassTPinnedDecl, TClass, TPinnedDecl, Type}

trait CoerceTClassTPinnedDeclImpl[G] { this: CoerceTClassTPinnedDecl[G] =>
  override def target: Type[G] = TPinnedDecl(pin, Nil)
}
