package vct.col.ast.statement.terminal

import vct.col.ast.{AssignStmt, EndpointName, Expr, Local}
import vct.col.check.{CheckContext, CheckError, SeqProgEndpointAssign}
import vct.col.print._
import vct.col.origin.{Blame, AssignFailed}

trait AssignStmtImpl[G]
    extends NormallyCompletingStatementImpl[G] {
  this: AssignStmt[G] =>

  val target: Expr[G]
  val value: Expr[G]
  val blame: Blame[AssignFailed]

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++
      (target match {
        case Local(ref) =>
          context.checkInWriteScope(context.roScopeReason, this, ref)
        case EndpointName(_) if context.currentChoreography.isDefined =>
          Seq(SeqProgEndpointAssign(this))
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
