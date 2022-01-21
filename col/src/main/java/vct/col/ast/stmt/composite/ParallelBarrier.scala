package vct.col.ast.stmt.composite

import java.util.ArrayList

import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.Contract
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, VisitorHelper}

class ParallelBarrier (val label:String, val contract:Contract, private[this] val fences:ArrayList[String], val body:BlockStatement) extends ASTNode with VisitorHelper {
  val invs = new ArrayList[String](fences)
    
  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields: Iterable[String] = Seq("contract", "body")
  override def debugTreePropertyFields: Iterable[String] = Seq("label", "fences")
}
