package vct.col.ast

import java.util.ArrayList
import scala.collection.JavaConverters._
import vct.col.util.VisitorHelper

class ParallelAtomic private[this] (val block:BlockStatement, val synclist:ArrayList[ASTNode]) extends ASTNode with VisitorHelper {
  def this(block:BlockStatement, synclist:Array[ASTNode]) = this(block, new ArrayList[ASTNode](synclist.toList.asJava))
  
  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))
}