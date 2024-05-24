package vct.col.ast.family.pvlcommunicate

import vct.col.ast.{PVLAccess, Type}
import vct.col.ast.ops.{PVLAccessOps, PVLAccessFamilyOps}

trait PVLAccessImpl[G] extends PVLAccessOps[G] with PVLAccessFamilyOps[G] { this: PVLAccess[G] =>
  def fieldType: Type[G] = ref.get.decl.t
}
