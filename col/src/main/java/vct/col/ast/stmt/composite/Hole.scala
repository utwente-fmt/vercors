package vct.col.ast.stmt.composite

import vct.col.ast.generic.ASTNode
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, VisitorHelper}

class Hole(private[this] val nodes: ThreadLocal[ASTNode]) extends ASTNode with VisitorHelper {
  def get: ASTNode = nodes.get

  override def accept_simple[T, A](m: ASTMapping1[T, A], arg: A) = m.map(this, arg)

  override def accept_simple[T](v: ASTVisitor[T]) = handle_standard(() => v.visit(this))

  override def accept_simple[T](m: ASTMapping[T]) = handle_standard(() => m.map(this))

  override def `match`(ast: ASTNode) = {
    nodes.set(ast);
    true
  }

  override def debugTreeChildrenFields: Iterable[String] = Seq("nodes")

  override def debugTreePropertyFields: Iterable[String] = Seq()
}