package vct.col.ast

import java.util.ArrayList
import scala.collection.JavaConverters._
import vct.col.util.VisitorHelper

class ParallelBarrier (val label:String, val contract:Contract, private[this] val fences:ArrayList[String], val body:BlockStatement) extends ASTNode with VisitorHelper {
  val invs = new ArrayList[String](fences)
    
  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))
}
