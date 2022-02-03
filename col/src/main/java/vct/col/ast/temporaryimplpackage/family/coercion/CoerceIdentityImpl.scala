package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceIdentity, Type}

trait CoerceIdentityImpl[G] { this: CoerceIdentity[G] =>
  override def target: Type[G] = source
}
