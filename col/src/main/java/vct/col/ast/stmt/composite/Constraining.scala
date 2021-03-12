package vct.col.ast.stmt.composite

import vct.col.ast.expr.NameExpression
import vct.col.ast.generic.ASTNode
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, VisitorHelper}

import scala.jdk.CollectionConverters._

case class Constraining(val block:BlockStatement, val vars:List[NameExpression]) extends ASTNode with VisitorHelper {
  require(vars != null, "The list of (constraining) vars is null.")

  /** Constructs a new constraining block from an array of variables.  */
  def this(block:BlockStatement, vars:Array[NameExpression]) = this(block, vars.toList)

  /** Gives a Java wrapper (over `java.util.List`) for the variable list. */
  def varsJava = vars.asJava

  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields(): Iterable[String] = Seq("vars", "block")
  override def debugTreePropertyFields(): Iterable[String] = Seq()
}
