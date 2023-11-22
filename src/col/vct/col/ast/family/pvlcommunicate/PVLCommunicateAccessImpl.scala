package vct.col.ast.family.pvlcommunicate

import vct.col.ast.{PVLAccess, Type}

trait PVLCommunicateAccessImpl[G] { this: PVLAccess[G] =>
  def fieldType: Type[G] = ref.get.decl.t
}
