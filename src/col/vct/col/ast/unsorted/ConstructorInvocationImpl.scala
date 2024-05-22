package vct.col.ast.unsorted

import vct.col.ast.{ConstructorInvocation, TClass, Type, Variable, Class}
import vct.col.ast.ops.ConstructorInvocationOps
import vct.col.print._

trait ConstructorInvocationImpl[G] extends ConstructorInvocationOps[G] { this: ConstructorInvocation[G] =>

  def cls: Class[G] = ref.decl.cls.decl

  override def typeEnv: Map[Variable[G], Type[G]] =
    (cls.typeArgs.zip(classTypeArgs) ++ ref.decl.typeArgs.zip(typeArgs)).toMap

   override def layout(implicit ctx: Ctx): Doc = {
     Doc.spread(Seq(
       Text("new"),
       DocUtil.javaGenericArgs(typeArgs),
       Text(ctx.deref(ref).map(_.cls).map(ctx.deref).flatten.map(ctx.name(_)).merge) <> DocUtil.javaGenericArgs(classTypeArgs) <>
         "(" <> DocUtil.argsOutArgs(args, outArgs) <> ")" <>
         DocUtil.givenYields(givenMap, yields)
     ))
   }
}
