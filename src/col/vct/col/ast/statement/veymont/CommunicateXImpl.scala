package vct.col.ast.statement.veymont

import vct.col.ast.CommunicateX
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.CommunicateXOps

trait CommunicateXImpl[G] extends CommunicateXOps[G] {
  this: CommunicateX[G] =>
  override def layout(implicit ctx: Ctx): Doc = assign.show
}
