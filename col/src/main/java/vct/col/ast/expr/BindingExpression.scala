package vct.col.ast.expr

import vct.col.ast.`type`.Type
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.DeclarationStatement
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor}

case class BindingExpression(binder: Binder, result_type: Type, decls: Seq[DeclarationStatement], triggers: Seq[Seq[ASTNode]], select: ASTNode, main: ASTNode) extends ExpressionNode {
  def this(binder: Binder, result_type: Type, decls: Array[DeclarationStatement], triggers: Array[Array[ASTNode]], select: ASTNode, main: ASTNode) =
    this(binder, result_type, decls.clone.toSeq, if (triggers == null) null else triggers.map(_.clone.toSeq).toSeq, select, main)

  override def debugTreeChildrenFields = Seq("select", "main")

  override def debugTreePropertyFields = Seq("binder", "result_type")

  def getDeclCount: Int = decls.size

  def getDeclaration(i: Int): DeclarationStatement = decls(i)

  def getDeclarations: Array[DeclarationStatement] = decls.toArray.clone

  def javaTriggers: Array[Array[ASTNode]] =
    if (triggers == null) null else triggers.map(_.toArray.clone).toArray

  override def accept_simple[T](visitor: ASTVisitor[T]) = visitor.visit(this)

  override def accept_simple[T](map: ASTMapping[T]) = map.map(this)

  override def accept_simple[R, A](map: ASTMapping1[R, A], arg: A) = map.map(this, arg)
}