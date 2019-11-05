package vct.col.ast.expr.constant

import scala.collection.JavaConverters._
import scala.collection.JavaConversions.mapAsScalaMap
import vct.col.ast.`type`.Type
import vct.col.ast.expr.ExpressionNode
import vct.col.ast.generic.ASTNode
import vct.col.ast.util.ASTVisitor
import vct.col.util.{ASTMapping, ASTMapping1, VisitorHelper}

import scala.collection.immutable.Map

case class StructValue(val `type`:Type, val map:Map[String,Integer], val values:List[ASTNode]) extends ExpressionNode with VisitorHelper {
  require(values != null, "The StructValue value list cannot be null")
  require(map != null, "The StructValue map cannot be null")

  def this(t:Type, map:Map[String,Integer], values:Array[ASTNode]) = this(t, map, values.toList)
  def this(t:Type, map:Map[String,Integer]) = this(t, map, Array[ASTNode]())
  def this(t:Type, map:java.util.Map[String,Integer], values:Array[ASTNode]) = this(t, map.toMap, values.toList)
  def this(t:Type, map:java.util.Map[String,Integer]) = this(t, map, Array[ASTNode]())
  def this(t:Type) = this(t, Map[String,Integer]())

  @deprecated("this method will be removed", "soon")
  def valuesLength = values.length

  @deprecated("this method will be removed", "soon")
  def value(i:Int) = values.apply(i)

  @deprecated("this method will be removed", "soon")
  def valuesArray = values.toArray

  /** Provides a Java wrapper (as `java.util.Map`) for `map` */
  def mapJava = map.asJava

  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields(): Iterable[String] = Seq("type", "map", "values")
  override def debugTreePropertyFields(): Iterable[String] = Seq()
}
