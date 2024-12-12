package vct.col.ast.family.pvlcommunicate

import vct.col.ast.PVLCommunicate
import vct.col.ast.ops.PVLCommunicateOps
import vct.col.ast.ops.{PVLCommunicateOps, PVLCommunicateFamilyOps}

trait PVLCommunicateImpl[G]
    extends PVLCommunicateOps[G] with PVLCommunicateFamilyOps[G] {
  this: PVLCommunicate[G] =>

}
