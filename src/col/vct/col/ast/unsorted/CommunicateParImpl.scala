package vct.col.ast.unsorted

import vct.col.ast.CommunicatePar
import vct.col.ast.ops.CommunicateParOps
import vct.col.print._

trait CommunicateParImpl[G] extends CommunicateParOps[G] {
  this: CommunicatePar[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
