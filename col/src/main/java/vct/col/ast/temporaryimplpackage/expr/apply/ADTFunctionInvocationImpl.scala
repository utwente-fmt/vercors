package vct.col.ast.temporaryimplpackage.expr.apply

import vct.col.ast.{ADTFunctionInvocation, Type}

trait ADTFunctionInvocationImpl { this: ADTFunctionInvocation =>
  override def t: Type =
    typeArgs match {
      case Some((adt, typeArgs)) =>
        ref.decl.returnType.particularize(adt.decl.typeArgs.zip(typeArgs).toMap)
      case None => ref.decl.returnType
    }
}
