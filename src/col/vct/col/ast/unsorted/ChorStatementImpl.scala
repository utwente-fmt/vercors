package vct.col.ast.unsorted

import vct.col.ast.{
  Block,
  Branch,
  ChorStatement,
  CommunicateStatement,
  EndpointStatement,
  Label,
  Loop,
  Node,
  Scope,
}
import vct.col.ast.ops.ChorStatementOps
import vct.col.print._

trait ChorStatementImpl[G] extends ChorStatementOps[G] {
  this: ChorStatement[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("/* choreographic statement */") <+/> inner
  assert(wellformed)
  def wellformed: Boolean =
    inner match {
      // There are only a few statements where we fully define how projection works - for now
      // Probably this fits better in the check pass, but it's fine for now
      case _: Branch[G] | _: Loop[G] => true
      case _ => false
    }
}
