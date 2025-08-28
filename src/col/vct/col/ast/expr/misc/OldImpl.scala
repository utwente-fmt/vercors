package vct.col.ast.expr.misc

import vct.col.ast.expr.binder.PossibleTriggerImpl
import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.{ContractApplicable, Old, PossibleTrigger, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.OldOps
import vct.col.check.{
  CheckContext,
  CheckError,
  OldInFunctionContract,
  OldInPrecondition,
}

trait OldImpl[G]
    extends OldOps[G] with NodeFamilyImpl[G] with PossibleTriggerImpl[G] {
  this: Old[G] =>
  override def t: Type[G] = expr.t

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    (ctx.syntax, at) match {
      case (Ctx.Silver, None) => Text("old(") <> expr <> ")"
      case (Ctx.Silver, Some(at)) =>
        Text("old[") <> ctx.name(at) <> "](" <> expr <> ")"
      case (_, None) => Text("\\old(") <> expr <> ")"
      case (_, Some(at)) =>
        Text("\\old[") <> ctx.name(at) <> "](" <> expr <> ")"
    }

  override def check(context: CheckContext[G]): Seq[CheckError] = {
    var result = super.check(context)
    if (
      context.currentApplicable.exists {
        case a: ContractApplicable[G] => a.pure
        case _ => false
      }
    )
      result = result :+ OldInFunctionContract(this)
    else if (context.inPreCondition)
      result = result :+ OldInPrecondition(this)

    result
  }

  override def isPossibleTrigger: Boolean =
    expr match {
      case t: PossibleTrigger[G] => t.isPossibleTrigger
      case _ => false
    }
}
