package vct.col.ast.unsorted

import vct.col.ast.CommunicateStatement
import vct.col.ast.ops.CommunicateStatementOps
import vct.col.print._

trait CommunicateStatementImpl[G] extends CommunicateStatementOps[G] {
  this: CommunicateStatement[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("/* Communicate container */") <+/> inner
}
