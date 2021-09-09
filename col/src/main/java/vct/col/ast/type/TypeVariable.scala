package vct.col.ast.`type`

import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, TypeMapping}
import vct.col.ast.util.VisitorHelper

object TypeVariable {
  val inferTypeName = "INFER_ADT_TYPE"
  val inferTypeVar: TypeVariable = TypeVariable(inferTypeName)
}

case class TypeVariable(val name:String) extends Type with VisitorHelper {
  override def isNumeric = false
  override def hashCode() = name.hashCode()
  override def supertypeof(context:ProgramUnit, t:Type) = false

  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))
  override def accept_simple[T](m:TypeMapping[T]) = handle_standard(() => m.map(this))

  override def equals(o:Any) = o match {
    case tv:TypeVariable => tv.name.equals(this.name)
    case _ => false
  }

  override def debugTreeChildrenFields: Iterable[String] = Seq("args")
  override def debugTreePropertyFields: Iterable[String] = Seq("name")
}
