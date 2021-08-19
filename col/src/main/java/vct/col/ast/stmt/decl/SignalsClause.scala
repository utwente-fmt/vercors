package vct.col.ast.stmt.decl

import vct.col.ast.`type`.Type
import vct.col.ast.generic.{ASTNode, DebugNode}
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, VisitorHelper}

case class SignalsClause(val name: String, val `type`: Type, val condition: ASTNode) extends ASTNode with VisitorHelper {
  override def getType: Type = this.`type`;

  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields: Iterable[String] = Seq("type", "condition")
  override def debugTreePropertyFields: Iterable[String] = Seq("name")

  /**
    * Turns the signals clause into a separate decl statement. Useful for use with ASTFrame, where a DeclarationStatement
    * is needed even for names with types that are themselves not DeclarationStatements.
    */
  def asDeclarationStatement: DeclarationStatement = DeclarationStatement(name, `type`, Option.empty)
}
