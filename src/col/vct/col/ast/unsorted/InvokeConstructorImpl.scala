package vct.col.ast.unsorted

import vct.col.ast.{Class, InvokeConstructor, Type, Variable}
import vct.col.ast.ops.InvokeConstructorOps
import vct.col.print._

trait InvokeConstructorImpl[G] extends InvokeConstructorOps[G] {
  this: InvokeConstructor[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???

  def cls: Class[G] = ref.decl.cls.decl

  def typeEnv: Map[Variable[G], Type[G]] =
    (cls.typeArgs.zip(classTypeArgs) ++ ref.decl.typeArgs.zip(typeArgs)).toMap
}
