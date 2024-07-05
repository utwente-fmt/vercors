package vct.col.ast.unsorted

import vct.col.ast.PVLCommunicateStatement
import vct.col.ast.ops.PVLCommunicateStatementOps
import vct.col.print._

trait PVLCommunicateStatementImpl[G] extends PVLCommunicateStatementOps[G] {
  this: PVLCommunicateStatement[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
