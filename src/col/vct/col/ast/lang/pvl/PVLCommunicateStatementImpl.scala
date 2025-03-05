package vct.col.ast.lang.pvl

import vct.col.ast.{
  PVLCommTargetRange,
  PVLCommunicateStatement,
  RangeBinder,
  Variable,
}
import vct.col.ast.ops.PVLCommunicateStatementOps
import vct.col.ast.statement.StatementImpl
import vct.col.check.{Check, CheckContext, CheckError}

trait PVLCommunicateStatementImpl[G]
    extends PVLCommunicateStatementOps[G] with StatementImpl[G] {
  this: PVLCommunicateStatement[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???

  def rangeBinder: Option[Variable[G]] = {
    // TODO (RR): Take inference into account here somehow?
    comm.sender match {
      case Some(PVLCommTargetRange(_, RangeBinder(v, _, _))) => Some(v)
      case _ => None
    }
  }

  // TODO (RR): This allows nodes[i := i .. i], which should instead be an error
  override def enterCheckContextScopes(
      context: CheckContext[G]
  ): Seq[CheckContext.ScopeFrame[G]] = {
    rangeBinder match {
      case Some(v) => context.withScope(Seq(v))
      case None => context.scopes
    }
  }
}
