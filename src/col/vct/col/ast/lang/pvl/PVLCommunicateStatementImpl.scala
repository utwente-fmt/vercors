package vct.col.ast.lang.pvl

import vct.col.ast.PVLCommunicateStatement
import vct.col.ast.ops.PVLCommunicateStatementOps

trait PVLCommunicateStatementImpl[G] extends PVLCommunicateStatementOps[G] {
  this: PVLCommunicateStatement[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
