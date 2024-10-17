package vct.col.ast.unsorted

import vct.col.ast.expr.ExprImpl
import vct.col.ast.{Class, ConstructorInvocation, TClass, Type, Variable}
import vct.col.ast.ops.ConstructorInvocationOps
import vct.col.check.{CheckContext, CheckError, TypeErrorExplanation}
import vct.col.print._

import scala.util.Try

trait ConstructorInvocationImpl[G]
    extends ConstructorInvocationOps[G] with ExprImpl[G] {
  this: ConstructorInvocation[G] =>

  def cls: Class[G] = ref.decl.cls.decl

  override def typeEnv: Map[Variable[G], Type[G]] =
    (cls.typeArgs.zip(classTypeArgs) ++ ref.decl.typeArgs.zip(typeArgs)).toMap

  override def layout(implicit ctx: Ctx): Doc = {
    Doc.spread(Seq(
      Text("new"),
      DocUtil.javaGenericArgs(typeArgs),
      Text(Try(cls).map(ctx.name(_)).getOrElse("?brokenref?")) <>
        DocUtil.javaGenericArgs(classTypeArgs) <> "(" <>
        DocUtil.argsOutArgs(args, outArgs) <> ")" <>
        DocUtil.givenYields(givenMap, yields),
    ))
  }

  override def precedence: Int = Precedence.ATOMIC

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++
      (if (classTypeArgs.size != cls.typeArgs.size)
         Seq(TypeErrorExplanation(
           this,
           "Number of class type arguments does not match number of type arguments at class definition",
         ))
       else
         Seq()) ++
      (if (typeArgs.size != ref.decl.typeArgs.size)
         Seq(TypeErrorExplanation(
           this,
           "Number of type arguments of constructor invocation does not match number of type arguments at constructor definition",
         ))
       else
         Seq())
}
