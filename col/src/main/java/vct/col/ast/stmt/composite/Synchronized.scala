package vct.col.ast.stmt.composite

import vct.col.ast.generic.ASTNode
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, VisitorHelper}

case class Synchronized(expr: ASTNode, statement: ASTNode) extends ASTNode with VisitorHelper {
  override def accept_simple[T](visitor: ASTVisitor[T]): Unit = visitor.visit(this)

  override def accept_simple[T](map: ASTMapping[T]): T = map.map(this)

  override def accept_simple[R, A](map: ASTMapping1[R, A], arg: A): R = map.map(this, arg)

  override def debugTreeChildrenFields: Iterable[String] = Seq("expr", "statement")

  override def debugTreePropertyFields: Iterable[String] = Seq()
}
