package vct.col.ast.stmt.composite

import vct.col.ast.generic.ASTNode
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, VisitorHelper}

import scala.jdk.CollectionConverters._
import scala.collection.immutable.Map

/**
 * AST node that represent an action block for use in e.g. Histories and Futures/Processes.
 *
 * @author sccblom, whmoortwijn
 * @param history the identifier and details of the histories predicate?
 * @param fraction The fraction of the history predicate?
 * @param process The process representing the Histories or Future
 * @param action The name and details of the action protecting this block
 * @param map A mapping from identifiers (process variable names) to heap locations?
 * @param block The contents (statement block) of this action block
 */
case class ActionBlock(val history:ASTNode, val fraction:ASTNode, val process:ASTNode, val action:ASTNode, val map:Map[String,ASTNode], val block:ASTNode) extends ASTNode with VisitorHelper {
  require(map != null, "The action block mapping is null")

  /** Added to retain compatibility with Java (by converting `map` to a Scala structure) */
  def this(history:ASTNode, fraction:ASTNode, process:ASTNode, action:ASTNode, map:java.util.Map[String,ASTNode], block:ASTNode) =
    this(history, fraction, process, action, map.asScala.toMap, block)

  /** Iterates over all key/value pairs in `map` and applies `f` on each pair */
  def foreach(f:(String,ASTNode)=>Unit) = map.foreach { case (k,v) => f(k,v) }

  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields: Iterable[String] = Seq("history", "fraction", "process", "action", "map", "block")
  override def debugTreePropertyFields: Iterable[String] = Seq()
}
