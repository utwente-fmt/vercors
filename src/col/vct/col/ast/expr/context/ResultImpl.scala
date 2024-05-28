package vct.col.ast.expr.context

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.{Result, Type}
import vct.col.check.{CheckContext, CheckError, ResultOutsidePostcondition}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.ResultOps

trait ResultImpl[G] extends NodeFamilyImpl[G] with ResultOps[G] {
  this: Result[G] =>
  override def t: Type[G] = applicable.decl.returnType

  override def check(context: CheckContext[G]): Seq[CheckError] =
    if (context.inPostCondition)
      super.check(context)
    else
      Seq(ResultOutsidePostcondition(this))

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => Text("result")
      case _ => Text("\\result")
    }
}
