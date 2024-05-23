package vct.col.ast.family.coercion

import vct.col.ast.{CoerceIdentity, Type}
import vct.col.ast.ops.CoerceIdentityOps

trait CoerceIdentityImpl[G] extends CoerceIdentityOps[G] { this: CoerceIdentity[G] =>
  override def target: Type[G] = source
}
