package vct.col.ast.lang

import vct.col.ast.{JavaClass, JavaClassOrInterface, JavaStringLiteral, JavaTClass, Type}
import vct.result.VerificationError.Unreachable

trait JavaStringLiteralImpl[G] { this: JavaStringLiteral[G] =>
  def t: Type[G] = JavaTClass[G](typeRef.getOrElse(throw Unreachable("java.lang.String base class not found"))
    .asInstanceOf[JavaClassOrInterface[G]]
    .ref[JavaClassOrInterface[G]], Seq()) // TODO (RR): Better exception
}
