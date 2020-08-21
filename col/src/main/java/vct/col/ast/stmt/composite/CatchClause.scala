package vct.col.ast.stmt.composite

import scala.collection.JavaConverters._

import vct.col.ast.`type`.Type
import vct.col.ast.generic.{ASTNode, DebugNode}
import vct.col.ast.stmt.decl.DeclarationStatement
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, VisitorHelper}


/**
 * Represents a catch-clause for use in a try-catch-finally block, for example:
 * "`catch (ExceptionType e) { S }`", with "`S`" the catch-clause body.
 *
 * @param block The body statement block of the catch clause (e.g. the handler body "`S`").
 */
case class CatchClause(name: String, catchTypes: Seq[Type], block: BlockStatement) extends ASTNode with VisitorHelper {
  def this(name: String, catchTypes: Array[Type], block: BlockStatement) = this(name, catchTypes.toSeq, block)

  def javaCatchTypes: Array[Type] = catchTypes.toArray

  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields: Iterable[String] = Seq("decl", "block")
  override def debugTreePropertyFields: Iterable[String] = Seq()
}
