package vct.col.ast.statement.terminal

import vct.col.ast.{Assign, Local}
import vct.col.check.{CheckContext, CheckError}

trait AssignImpl[G] extends NormallyCompletingStatementImpl[G] { this: Assign[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++ (target match {
      case Local(ref) => context.checkInWriteScope(context.roScopeReason, this, ref)
      case _ => Nil
    })
}