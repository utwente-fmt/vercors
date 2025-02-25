package vct.col.ast.family.pvlcommunicate

import vct.col.ast.PVLEndpointName
import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.ops.PVLEndpointNameOps
import vct.col.ast.ops.{PVLEndpointNameFamilyOps, PVLEndpointNameOps}
import vct.col.print.{Ctx, Doc, Text}

trait PVLEndpointNameImpl[G]
    extends PVLEndpointNameOps[G]
    with PVLEndpointNameFamilyOps[G]
    with NodeFamilyImpl[G] {
  this: PVLEndpointName[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text(name)
}
