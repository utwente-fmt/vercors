package vct.col.ast.unsorted

import vct.col.ast.{CommunicateTarget, Endpoint}
import vct.col.print._
import vct.col.ast.ops.CommunicateTargetFamilyOps
import vct.col.ref.Ref

trait CommunicateTargetImpl[G] extends CommunicateTargetFamilyOps[G] {
  this: CommunicateTarget[G] =>
//  override def layout(implicit ctx: Ctx): Doc = this.layout

  def ref: Ref[G, Endpoint[G]]
  def endpoint: Endpoint[G] = ref.decl
}
