package vct.col.ast

import vct.col.util.VisitorHelper

class TupleType(private[this] val _types:Seq[Type]) extends Type with VisitorHelper {
  val types = _types.toArray
  
  def this(_types:Array[Type]) = this(_types.toSeq)
  def `type`(i:Int) = types.apply(i)

  override def supertypeof(context:ProgramUnit, t:Type) = false
  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))
  override def accept_simple[T](m:TypeMapping[T]) = handle_standard(() => m.map(this))
}
