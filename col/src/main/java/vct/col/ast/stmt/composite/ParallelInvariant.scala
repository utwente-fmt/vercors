package vct.col.ast.stmt.composite

import vct.col.ast.generic.ASTNode
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, VisitorHelper}

case class ParallelInvariant(val label:String, val inv:ASTNode, val block:BlockStatement) extends ASTNode with VisitorHelper {
  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) =  handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields: Iterable[String] = Seq("inv", "block")
  override def debugTreePropertyFields: Iterable[String] = Seq("label")
}
