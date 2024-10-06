package vct.col.ast.unsorted

import vct.col.ast.{SimplifiedProcedureInvocation, Type, Variable}
import vct.col.ast.ops.SimplifiedProcedureInvocationOps
import vct.col.print._

trait SimplifiedProcedureInvocationImpl[G]
    extends SimplifiedProcedureInvocationOps[G] {
  this: SimplifiedProcedureInvocation[G] =>
  override def precedence: Int = Precedence.POSTFIX

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Group(
        Text(ctx.name(ref)) <>
          (if (typeArgs.nonEmpty)
             Text("<") <> Doc.args(typeArgs) <> ">"
           else
             Empty) <> "("
      ) <> Doc.args(args ++ outArgs) <> ")" <>
        DocUtil.givenYields(givenMap, yields)
    )

  override def typeEnv: Map[Variable[G], Type[G]] =
    ref.decl.typeArgs.zip(typeArgs).toMap
}
