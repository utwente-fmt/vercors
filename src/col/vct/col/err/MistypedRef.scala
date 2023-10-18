package vct.col.err

import vct.col.ast.Declaration

import scala.reflect.ClassTag

case class MistypedRef(received: Declaration[_], expected: ClassTag[_])
    extends ASTStateError {
  override def text: String =
    "A reference in the AST is referencing a declaration of the wrong kind.\n" +
      s"A ${expected.runtimeClass.getSimpleName} was expected here, but we got a ${received.getClass.getSimpleName}"
}
