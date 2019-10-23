package vct.col.ast.`type`

import scala.collection.JavaConverters._
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.util.{ASTVisitor, TypeMapping}
import vct.col.util.{ASTMapping, ASTMapping1, VisitorHelper}

case class TypeExpression(val operator:TypeOperator, val types:List[Type]) extends Type with VisitorHelper {
  require(types != null, "The types list is null")

  /** Constructs a new type expression from an array of types */
  def this(operator:TypeOperator, types:Array[Type]) = this(operator, types.toList)

  /** Gives the heading type in the type list */
  def firstType = types.head

  /** Provides a Java wrapper (as `java.util.List`) over the types list (`types`) */
  def typesJava = types.asJava

  override def isNumeric = operator match {
    case TypeOperator.Local | TypeOperator.Global | TypeOperator.Long => firstType.isNumeric
    case _ => false
  }

  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))
  override def accept_simple[T](m:TypeMapping[T]) = handle_standard(() => m.map(this))
  override def supertypeof(context:ProgramUnit, t:Type) = false

  override def debugTreeChildrenFields: Iterable[String] = Seq("types", "args")
  override def debugTreePropertyFields: Iterable[String] = Seq("operator")
}
