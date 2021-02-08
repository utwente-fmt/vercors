package vct.col.ast.stmt.decl

import vct.col.ast.generic.ASTNode
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor}

object GPUOptName extends Enumeration {
  val LoopUnroll = Value
}

case class GPUOpt(val name:GPUOptName.Value, val args: List[ASTNode]) extends ASTNode {

    override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
    override def accept_simple[T](v:ASTVisitor[T]) = v.visit(this)
    override def accept_simple[T](m:ASTMapping[T]) = m.map(this)

    override def debugTreeChildrenFields: Iterable[String] = Seq("name", "args")

    override def debugTreePropertyFields: Iterable[String] = Seq()
  }
