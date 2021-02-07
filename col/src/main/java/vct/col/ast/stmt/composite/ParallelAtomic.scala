package vct.col.ast.stmt.composite

import vct.col.ast.generic.{ASTNode, BeforeAfterAnnotations}
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, VisitorHelper}

import scala.collection.JavaConverters._

case class ParallelAtomic(val block:ASTNode, val synclist:List[ASTNode]) extends ASTNode with VisitorHelper with BeforeAfterAnnotations {
  private var before: BlockStatement = new BlockStatement
  private var after: BlockStatement = new BlockStatement

  require(synclist != null, "The list of synchronisation elements is null")

  /** Constructs a parallel atomic block from an array of synchronisation elements */
  def this(block:ASTNode, syncarray:Array[ASTNode]) = this(block, syncarray.toList)

  /** Yields a Java wrapper (for `java.util.List`) over the `synclist` collection  */
  def synclistJava = synclist.asJava

  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields(): Iterable[String] = Seq("synclist", "block")
  override def debugTreePropertyFields(): Iterable[String] = Seq()

  override def set_before(block: BlockStatement): BeforeAfterAnnotations = { before = block; this }
  override def get_before: BlockStatement = before
  override def set_after(block: BlockStatement): BeforeAfterAnnotations = { after = block; this }
  override def get_after: BlockStatement = after
}
