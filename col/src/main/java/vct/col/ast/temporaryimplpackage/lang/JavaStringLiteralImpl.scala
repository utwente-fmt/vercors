package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{CType, CompositeType, DeclaredType, JavaClass, JavaNamedType, JavaStringLiteral, JavaTClass, JavaType, PVLType, PrimitiveType, SilverType, TArray, TNotAValue, TPointer, TType, TUnion, TVar, Type}
import vct.col.resolve.Java
import vct.result.VerificationResult.Unreachable

trait JavaStringLiteralImpl[G] { this: JavaStringLiteral[G] =>
  override def t: Type[G] = base

  def get: JavaClass[G] = base match {
    case base: JavaNamedType[G] => base.ref.get.asInstanceOf[JavaClass[G]]
    case base: JavaTClass[G] => base.ref.decl.asInstanceOf[JavaClass[G]]
  }
}
