package vct.col.ast.stmt.composite

import vct.col.ast.`type`.Type
import vct.col.ast.generic.DebugNode
import vct.col.ast.stmt.decl.DeclarationStatement

/**
 * Represents a catch-clause for use in a try-catch-finally block, for example:
 * "`catch (ExceptionType e) { S }`", with "`S`" the catch-clause body.
 *
 * @param block The body statement block of the catch clause (e.g. the handler body "`S`").
 */
case class CatchClause(name: String, catchTypes: Seq[Type], block: BlockStatement) extends DebugNode {
  def javaCatchTypes: Array[Type] = catchTypes.toArray

  override def debugTreeChildrenFields: Iterable[String] = Seq("decl", "block")
  override def debugTreePropertyFields: Iterable[String] = Seq()
}
