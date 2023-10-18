package vct.col.ast.statement.terminal

import vct.col.ast.{Assign, Local}
import vct.col.check.{CheckContext, CheckError}
import vct.col.print._

trait AssignImpl[G] extends NormallyCompletingStatementImpl[G] {
  this: Assign[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++
      (target match {
        case Local(ref) =>
          context.checkInWriteScope(context.roScopeReason, this, ref)
        case _ => Nil
      })

  def layoutAsExpr(implicit ctx: Ctx): Doc =
    Group(
      target.show <+>
        (if (ctx.syntax == Ctx.Silver)
           ":="
         else
           "=") <>> value
    )

  override def layout(implicit ctx: Ctx): Doc =
    layoutAsExpr <>
      (if (ctx.syntax == Ctx.Silver)
         Empty
       else
         Text(";"))
}
