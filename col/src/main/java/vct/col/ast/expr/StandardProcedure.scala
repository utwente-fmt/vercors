package vct.col.ast.expr

import vct.col.ast.generic.ASTNode
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, VisitorHelper}

/**
  *Simple class that wraps a `StandardOperator` as an AST node.
  *Included in the design to support a future extension into functional languages:
 **
 *`OperatorExpression(op, args) == ApplyExpression(StandardProcedure(op), args)`
 **
 *
 *@param `operator` The (standard) operator that is wrapped.
  *@author sccblom, whmoortwijn
 */
case class StandardProcedure(val operator:StandardOperator) extends ASTNode with VisitorHelper {
  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields(): Iterable[String] = Seq()
  override def debugTreePropertyFields(): Iterable[String] = Seq("operator")
}
