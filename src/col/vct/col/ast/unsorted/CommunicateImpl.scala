package vct.col.ast.unsorted

import vct.col.ast.Communicate
import vct.col.ast.ops.CommunicateFamilyOps
import vct.col.print._

trait CommunicateImpl[G] extends CommunicateFamilyOps[G] {
  this: Communicate[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
