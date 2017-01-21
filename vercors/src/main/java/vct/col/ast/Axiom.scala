package vct.col.ast

import vct.col.util.VisitorHelper
import vct.util.ClassName

class Axiom(private[this] val _name:String, val rule:ASTNode) extends ASTDeclaration(_name) with VisitorHelper {
  override def getDeclName() = new ClassName(name)
  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))
}
