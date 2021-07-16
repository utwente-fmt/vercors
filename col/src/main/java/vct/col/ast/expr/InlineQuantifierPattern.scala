package vct.col.ast.expr

import vct.col.ast.generic.ASTNode
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, VisitorHelper}

case class InlineQuantifierPattern(inner: ASTNode) extends ASTNode with VisitorHelper {
  override def accept_simple[T](visitor: ASTVisitor[T]): Unit = handle_standard(() => visitor.visit(this))

  override def accept_simple[T](map: ASTMapping[T]): T = handle_standard(() => map.map(this))

  override def accept_simple[R, A](map: ASTMapping1[R, A], arg: A): R = handle_standard(() => map.map(this, arg))

  override def debugTreeChildrenFields: Iterable[String] = Seq("inner")

  override def debugTreePropertyFields: Iterable[String] = Seq()
}
