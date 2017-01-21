package vct.col.ast

import scala.collection.JavaConverters._
import vct.col.util.VisitorHelper

class ActionBlock(val history:ASTNode, val fraction:ASTNode, val process:ASTNode, val action:ASTNode, val map:Map[String,ASTNode], val block:ASTNode) extends ASTNode with VisitorHelper {
  def mapAsJava() = map.asJava
  
  /** This constructor is added to retain compatibility with Java (by converting the map to a Scala structure) */
  def this(history:ASTNode, fraction:ASTNode, process:ASTNode, action:ASTNode, map:java.util.Map[String,ASTNode], block:ASTNode) = 
    this(history, fraction, process, action, map.asScala.toMap, block)
  
  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))
}