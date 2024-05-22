package vct.col.ast.unsorted

import vct.col.ast.{ConstructorInvocation, TClass, Type, Variable, Class}
import vct.col.ast.ops.ConstructorInvocationOps
import vct.col.print._

trait ConstructorInvocationImpl[G] extends ConstructorInvocationOps[G] { this: ConstructorInvocation[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???

  def cls: Class[G] = ref.decl.cls.decl

//  override def t: Type[G] = TClass(ref.decl.cls, classTypeArgs)
  override def typeEnv: Map[Variable[G], Type[G]] =
    (cls.typeArgs.zip(classTypeArgs) ++ ref.decl.typeArgs.zip(typeArgs)).toMap


}
