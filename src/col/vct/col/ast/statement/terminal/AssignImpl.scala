package vct.col.ast.statement.terminal

import vct.col.ast.{Assign, Local}
import vct.col.check.{CheckContext, CheckError}
import vct.col.print.{Ctx, Doc, Show, Text, Empty}

trait AssignImpl[G] extends NormallyCompletingStatementImpl[G] { this: Assign[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++ (target match {
      case Local(ref) => context.checkInWriteScope(context.roScopeReason, this, ref)
      case _ => Nil
    })

  override def layout(implicit ctx: Ctx): Doc =
    target.show <+> (if(ctx.syntax == Ctx.Silver) ":=" else "=") <>> value <> (if(ctx.syntax == Ctx.Silver) Empty else Text(";"))
}