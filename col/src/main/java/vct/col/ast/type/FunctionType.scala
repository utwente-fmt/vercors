package vct.col.ast.`type`

import scala.jdk.CollectionConverters._
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, TypeMapping}
import vct.col.ast.util.VisitorHelper

/**
 * AST node representing function types (`T_0*...*T_n -> T`), where `params` is the
 * list of parameter types (`T_0,...,T_n`) and `result` the return type (`T`).
 *
 * @author sccblom, whmoortwijn
 */
case class FunctionType(val params:List[Type], val result:Type) extends Type with VisitorHelper {
  require(params != null, "The parameter list is null")
  require(result != null, "Function types should have a result type")

  /** Constructs a new function type from an array of parameter types */
  def this(params:Array[Type], result:Type) = this(params.toList, result)

  /** Constructs a new function type from Java constructs (for Java interoperability) */
  def this(params:java.util.List[Type], result:Type) = this(params.asScala.toList, result)

  /** Constructs a new unary function type */
  def this(param:Type, result:Type) = this(List(param), result)

  /** Provides a Java wrapper (as `java.util.List`) for the list of parameter types. */
  def paramsJava = params.asJava

  override def equals(other:Any) = other match {
    case FunctionType(`params`, `result`) => true
    case _ => false
  }

  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))
  override def accept_simple[T](m:TypeMapping[T]) = handle_standard(() => m.map(this))
  override def supertypeof(context:ProgramUnit, otherType:Type) = false
  override val zero = null

  override def debugTreeChildrenFields: Iterable[String] = Seq("params", "result", "args")
  override def debugTreePropertyFields: Iterable[String] = Seq()
}
