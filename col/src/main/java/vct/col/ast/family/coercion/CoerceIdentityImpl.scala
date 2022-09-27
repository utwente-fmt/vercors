package vct.col.ast.family.coercion

import vct.col.ast.{CoerceIdentity, Type}

trait CoerceIdentityImpl[G] { this: CoerceIdentity[G] =>
  override def target: Type[G] = source
}
