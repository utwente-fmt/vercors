package vct.col.ast.expr

import vct.col.ast.generic.{ASTNode}
import vct.col.ast.util.ASTVisitor
import vct.col.util.{ASTMapping, ASTMapping1}
import vct.util.ClassName

/**
 * AST node that represents an access to a field, e.g. "{@code obj.fieldname}".
 *
 * @param classname The fully qualified name of the class in which the variable is to be accessed,
 * e.g. the classname of "{@code obj}".
 * @param object The object the be accessed or {@code null} for a static variable, e.g. "{@code obj}".
 * @param name The name of the field to be accessed, e.g. "{@code fieldname}".
 * @param value This field is non-{@code null} for a write and {@code null} for a read.
 */
case class FieldAccess(val classname:ClassName, val `object`:ASTNode, val name:String, val value:ASTNode) extends ASTNode {
  override def accept_simple[T,A](map:ASTMapping1[T,A], arg:A) = map.map(this, arg)
  override def accept_simple[T](visitor:ASTVisitor[T]) = visitor.visit(this)
  override def accept_simple[T](map:ASTMapping[T]) = map.map(this)

  override def debugTreeChildrenFields(): Iterable[String] = Seq("object", "value")
  override def debugTreePropertyFields(): Iterable[String] = Seq("classname", "name")
}
