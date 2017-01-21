package vct.col.ast

import vct.col.util.VisitorHelper

class Hole(private[this] val nodes:ThreadLocal[ASTNode]) extends ASTNode with VisitorHelper {
  def get : ASTNode = nodes.get
  
  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))
  override def `match`(ast:ASTNode) = { nodes.set(ast); true }
}