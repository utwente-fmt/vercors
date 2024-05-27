package vct.col.ast.expr.apply

import vct.col.ast.{FunctionInvocation, Variable, Type}
import vct.col.print._
import vct.col.ast.ops.FunctionInvocationOps

trait FunctionInvocationImpl[G] extends FunctionInvocationOps[G] {
  this: FunctionInvocation[G] =>
  def layoutSilver(implicit ctx: Ctx): Doc =
    Group(Text(ctx.name(ref)) <> "(" <> Doc.args(args) <> ")")

  def layoutSpec(implicit ctx: Ctx): Doc =
    Group(
      Group(
        Text(ctx.name(ref)) <>
          (if (typeArgs.nonEmpty)
             Text("<") <> Doc.args(typeArgs) <> ">"
           else
             Empty) <> "("
      ) <> Doc.args(args) <> ")" <> DocUtil.givenYields(givenMap, yields)
    )

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => layoutSilver
      case _ => layoutSpec
    }

  override def typeEnv: Map[Variable[G], Type[G]] =
    ref.decl.typeArgs.zip(typeArgs).toMap
}
