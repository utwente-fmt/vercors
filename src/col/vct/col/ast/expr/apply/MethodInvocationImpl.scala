package vct.col.ast.expr.apply

import vct.col.ast.{MethodInvocation, TClass, Type}
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

  override def t: Type[G] = {
    val returnType = super.t
    obj.t match {
      case TClass(Ref(cls), typeArgs) if typeArgs.nonEmpty =>
        returnType.particularize(cls.typeArgs.zip(typeArgs).toMap)
      case _ => returnType
    }
  }
}
