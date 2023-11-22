package vct.col.ast.family.pvlcommunicate

import vct.col.ast.{PVLAccess, Type}

trait PVLAccessImpl[G] { this: PVLAccess[G] =>
  def fieldType: Type[G] = ref.get.decl.t
}
