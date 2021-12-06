package vct.col.ref

import vct.col.ast.Declaration
import vct.col.err.ASTStateError

import scala.reflect.ClassTag

case class MistypedRef(received: Declaration, expected: ClassTag[_]) extends ASTStateError {
  override def text: String =
    "A reference in the AST is referencing a declaration of the wrong kind.\n" +
      s"A ${expected.runtimeClass.getSimpleName} was expected here, but we got a ${received.getClass.getSimpleName}"
}
