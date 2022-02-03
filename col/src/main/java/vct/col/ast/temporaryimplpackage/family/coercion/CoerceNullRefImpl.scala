package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceNullRef, TRef}

trait CoerceNullRefImpl[G] { this: CoerceNullRef[G] => 
  override def target: TRef[G] = TRef()
}
