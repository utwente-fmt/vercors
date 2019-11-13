package vct.col.ast.stmt.decl

import vct.col.ast.`type`.Type
import vct.col.ast.generic.ASTNode
import vct.col.ast.util.ASTVisitor
import vct.col.util.{ASTMapping, ASTMapping1, VisitorHelper}

case class SignalsClause(val name: String, val `type`: Type, val condition: ASTNode) extends ASTNode with VisitorHelper {
  override def getType: Type = this.`type`;

  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields(): Iterable[String] = Seq("type", "init")
  override def debugTreePropertyFields(): Iterable[String] = Seq("name")
}
