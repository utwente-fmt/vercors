package vct.col.ast.family.pvlcommunicate

import vct.col.ast.{PVLCommunicateAccess, Type}

trait PVLCommunicateAccessImpl[G] { this: PVLCommunicateAccess[G] =>
  def fieldType: Type[G] = ref.get.decl.t
}
