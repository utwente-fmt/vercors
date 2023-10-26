package vct.col.ast.family.pvlcommunicate

import vct.col.ast.{PVLFamilyRange, TClass, Type}

trait PVLFamilyRangeImpl[G] { this: PVLFamilyRange[G] =>
  def t: Type[G] = ref.get.decl.t
}
