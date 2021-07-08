package vct.col.ast.stmt.composite

import vct.col.ast.generic.ASTNode

import scala.jdk.CollectionConverters._
import vct.col.ast.stmt.decl.{Contract, DeclarationStatement}
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, VisitorHelper}

case class ParallelBlock (val label:String, val contract:Contract, val iters:List[DeclarationStatement], val block:BlockStatement, val deps:Array[ASTNode]) extends ASTNode with VisitorHelper {
  require(deps != null, "dependency array is null")
  require(iters != null, "iteration list is null")

  /** Constructs a new parallel block from an array (`iters`) of iteration statements. */
  def this(label:String, contract:Contract, iters:Array[DeclarationStatement], block:BlockStatement, deps:Array[ASTNode]) = this(label, contract, iters.toList, block, deps)

  /** Constructs a new parallel block from Java constructs. */
  def this(label:String, contract:Contract, iters:java.util.List[DeclarationStatement], block:BlockStatement, deps:Array[ASTNode]) = this(label, contract, iters.asScala.toList, block, deps)

  /** Gives a Java wrapper (as `java.util.List`) over the list of iterations. */
  def itersJava = iters.asJava

  /** Yields the number of iteration statements. But beware, `iterslength` takes linear time, not constant time. */
  def iterslength = iters.length

  /** Yields the number of dependencies */
  def depslength = deps.length

  /** Gives the `i`-th dependency */
  def dependency(i:Int) = deps(i)

  /** Assigns `value` to the `i`-th dependency */
  def dependency(i:Int, value:ASTNode) = deps(i) = value

  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields: Iterable[String] = Seq("iters", "contract", "deps", "block")
  override def debugTreePropertyFields: Iterable[String] = Seq("label")
}
