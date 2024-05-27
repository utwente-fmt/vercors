package vct.col.ast.family.pvlcommunicate

import vct.col.ast.PVLEndpointName
import vct.col.ast.ops.PVLEndpointNameOps
import vct.col.ast.ops.{PVLEndpointNameOps, PVLEndpointNameFamilyOps}

trait PVLEndpointNameImpl[G]
    extends PVLEndpointNameOps[G] with PVLEndpointNameFamilyOps[G] {
  this: PVLEndpointName[G] =>
}
