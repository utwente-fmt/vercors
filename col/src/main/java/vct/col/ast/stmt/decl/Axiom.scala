package vct.col.ast.stmt.decl

import vct.col.ast._
import vct.col.ast.generic.ASTNode
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, ClassName, VisitorHelper}

case class Axiom(override val name:String, val rule:ASTNode) extends ASTDeclaration(name) with VisitorHelper {
  override def getDeclName() = new ClassName(name)
  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields(): Iterable[String] = Seq("rule")
  override def debugTreePropertyFields(): Iterable[String] = Seq("name")
}
