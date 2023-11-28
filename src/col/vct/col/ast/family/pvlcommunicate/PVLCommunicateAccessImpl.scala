package vct.col.ast.family.pvlcommunicate

import vct.col.ast.{PVLCommunicateAccess, Type}
import vct.col.ast.ops.{PVLCommunicateAccessOps, PVLCommunicateAccessFamilyOps}

trait PVLCommunicateAccessImpl[G] extends PVLCommunicateAccessOps[G] with PVLCommunicateAccessFamilyOps[G] { this: PVLCommunicateAccess[G] =>
  def fieldType: Type[G] = ref.get.decl.t
}
