package vct.col.ast.family.coercion

import vct.col.ast.{CoerceNullEnum, TEnum}
import vct.col.ast.ops.CoerceNullEnumOps

trait CoerceNullEnumImpl[G] extends CoerceNullEnumOps[G] { this: CoerceNullEnum[G] =>
  override def target: TEnum[G] = TEnum(targetEnum)
}
