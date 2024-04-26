package vct.col.ast.expr.apply

import vct.col.ast.{MethodInvocation, TClass, Type, Variable}
import vct.col.print.{Ctx, Doc, DocUtil, Empty, Group, Precedence, Text}
import vct.col.ast.ops.MethodInvocationOps
import vct.col.ref.Ref

trait MethodInvocationImpl[G] extends MethodInvocationOps[G] with InvocationImpl[G] { this: MethodInvocation[G] =>
  override def precedence: Int = Precedence.POSTFIX

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Group(
        assoc(obj) <> "." <> ctx.name(ref) <>
          (if (typeArgs.nonEmpty) Text("<") <> Doc.args(typeArgs) <> ">" else Empty) <>
          "("
      ) <> Doc.args(args ++ outArgs) <> ")" <> DocUtil.givenYields(givenMap, yields)
    )

  override def typeEnv: Map[Variable[G], Type[G]] = ref.decl.typeArgs.zip(typeArgs).toMap ++ obj.t.asClass.get.typeEnv
}
