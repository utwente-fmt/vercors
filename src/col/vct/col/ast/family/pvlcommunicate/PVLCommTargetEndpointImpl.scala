package vct.col.ast.family.pvlcommunicate

import vct.col.ast.{PVLCommTargetEndpoint, Type}
import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.ops.PVLCommTargetEndpointOps
import vct.col.print.{Ctx, Doc, Text}

trait PVLCommTargetEndpointImpl[G]
    extends PVLCommTargetEndpointOps[G] with NodeFamilyImpl[G] {
  this: PVLCommTargetEndpoint[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text(name)

  override def t: Type[G] = ref.get.decl.t
}
