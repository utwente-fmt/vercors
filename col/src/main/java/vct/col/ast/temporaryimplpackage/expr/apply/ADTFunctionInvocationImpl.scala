package vct.col.ast.temporaryimplpackage.expr.apply

import vct.col.ast.{ADTFunction, ADTFunctionInvocation, Type}
import vct.col.ref.Ref

trait ADTFunctionInvocationImpl[G] { this: ADTFunctionInvocation[G] =>
  override def ref: Ref[G, _ <: ADTFunction[G]]
  override def t: Type[G] =
    typeArgs match {
      case Some((adt, typeArgs)) =>
        ref.decl.returnType.particularize(adt.decl.typeArgs.zip(typeArgs).toMap)
      case None => ref.decl.returnType
    }
}
