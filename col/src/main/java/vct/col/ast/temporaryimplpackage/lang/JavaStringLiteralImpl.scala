package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{CType, CompositeType, DeclaredType, JavaClass, JavaLangString, JavaNamedType, JavaStringLiteral, JavaTClass, JavaType, PVLType, PrimitiveType, SilverType, TArray, TNotAValue, TPinnedDecl, TPointer, TType, TUnion, TVar, Type}
import vct.col.resolve.Java
import vct.result.VerificationResult.Unreachable

trait JavaStringLiteralImpl[G] { this: JavaStringLiteral[G] =>
  def t: TPinnedDecl[G] = TPinnedDecl[G](JavaLangString())
}
