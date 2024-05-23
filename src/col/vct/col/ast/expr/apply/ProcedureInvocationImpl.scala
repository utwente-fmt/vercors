package vct.col.ast.expr.apply

import vct.col.ast.{ProcedureInvocation, Type, Variable}
import vct.col.print._
import vct.col.ast.ops.ProcedureInvocationOps

trait ProcedureInvocationImpl[G] extends ProcedureInvocationOps[G] { this: ProcedureInvocation[G] =>
  override def precedence: Int = Precedence.POSTFIX

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Group(
        Text(ctx.name(ref)) <>
          (if (typeArgs.nonEmpty) Text("<") <> Doc.args(typeArgs) <> ">" else Empty) <>
          "("
      ) <> Doc.args(args ++ outArgs) <> ")" <> DocUtil.givenYields(givenMap, yields)
    )

  override def typeEnv: Map[Variable[G], Type[G]] = ref.decl.typeArgs.zip(typeArgs).toMap
}