package vct.col.ast.statement.exceptional

import vct.col.ast.statement.StatementImpl
import vct.col.ast.{Deref, Endpoint, EndpointUse, Eval, Expr, MethodInvocation, PVLDeref, PVLEndpoint, PVLLocal, Statement, ThisChoreography}
import vct.col.check.{CheckContext, CheckError, SeqProgInvocation}
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.EvalOps
import vct.col.ref.Ref
import vct.col.resolve.ctx.RefPVLEndpoint

trait EvalImpl[G] extends StatementImpl[G] with EvalOps[G] { this: Eval[G] =>
  override def layout(implicit ctx: Ctx): Doc = expr.show <> ";"
}