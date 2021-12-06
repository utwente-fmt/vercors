package vct.col.ref

import vct.col.ast.Declaration
import vct.col.err.ASTStateError

import scala.reflect.ClassTag

case class NotResolved(ref: UnresolvedRef[_ <: Declaration], expected: ClassTag[_]) extends ASTStateError {
  override def text: String =
    "The declaration of an unresolved reference was queried, but it is not yet resolved.\n" +
      s"We expected the name `${ref.name}` to resolve to a ${expected.runtimeClass.getSimpleName}."
}
