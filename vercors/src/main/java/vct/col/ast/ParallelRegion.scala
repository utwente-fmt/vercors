package vct.col.ast

import scala.annotation.varargs
import vct.col.util.VisitorHelper

/** Instead of calling the constructor, create new ParallelRegion object via {@code ParallelRegion.construct}. */
class ParallelRegion (val contract:Contract, private[this] val _blocks:Array[ParallelBlock]) extends ASTNode with VisitorHelper {
  val blocks = _blocks.clone()
  
  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))
}