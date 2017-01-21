package vct.col.ast

import vct.col.util.VisitorHelper

/**
  Simple class that wraps a StandardOperator as an ASTNode.
  Included in the design to support a future extension into functional languages:
  
  OperatorExpression(op,args) == ApplyExpression(StandardProcedure(op),args)
 */
class StandardProcedure(val operator:StandardOperator) extends ASTNode with VisitorHelper {
  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))
}