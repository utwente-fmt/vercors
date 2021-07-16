package vct.mock

import vct.col.ast.`type`.Type
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, TypeMapping}

class TypeMock(val _args: List[ASTNode]) extends Type(_args) {
  override def accept_simple[T, A](m: ASTMapping1[T, A], arg: A) = throw new Exception()

  override def accept_simple[T](v: ASTVisitor[T]) = throw new Exception()

  override def accept_simple[T](m: ASTMapping[T]) = throw new Exception()

  override def accept_simple[T](m: TypeMapping[T]) = throw new Exception()

  override def supertypeof(context: ProgramUnit, t: Type) = false

  override def debugTreeChildrenFields: Iterable[String] = Seq("_args")

  override def debugTreePropertyFields: Iterable[String] = Seq()
}
