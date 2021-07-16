package vct.col.ast.expr

import vct.col.ast.generic.ASTNode
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor}

case class KernelInvocation(method: String, blockCount: ASTNode, threadCount: ASTNode, args: Seq[ASTNode]) extends ExpressionNode {
  def this(method: String, blockCount: ASTNode, threadCount: ASTNode, args: Array[ASTNode]) =
    this(method, blockCount, threadCount, args.clone().toSeq)

  def javaArgs: Array[ASTNode] = args.toArray

  override def accept_simple[T](visitor: ASTVisitor[T]): Unit = visitor.visit(this)

  override def accept_simple[T](map: ASTMapping[T]): T = map.map(this)

  override def accept_simple[R, A](map: ASTMapping1[R, A], arg: A): R = map.map(this, arg)

  override def debugTreeChildrenFields: Iterable[String] = Seq()

  override def debugTreePropertyFields: Iterable[String] = Seq("method", "blockCount", "threadCount", "args")
}
