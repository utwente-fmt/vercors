package vct.col.ast.family.pvlcommunicate

import vct.col.ast.{PVLFamilyRange, TClass}

trait PVLFamilyRangeImpl[G] { this: PVLFamilyRange[G] =>
  override def threadType: TClass[G] = ???
}
