package vct.col.ast.family.coercion

import vct.col.ast.{CoerceNullEnum, TEnum}

trait CoerceNullEnumImpl[G] { this: CoerceNullEnum[G] =>
  override def target: TEnum[G] = TEnum(targetEnum)
}
