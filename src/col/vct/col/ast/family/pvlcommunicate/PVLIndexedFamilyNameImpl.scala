package vct.col.ast.family.pvlcommunicate

import vct.col.ast.{PVLIndexedFamilyName, TClass, Type}

trait PVLIndexedFamilyNameImpl[G] { this: PVLIndexedFamilyName[G] =>
  def t: Type[G] = ref.get.decl.t
}
