package vct.col.ast.`type`

import hre.lang.System.Abort
import vct.col.ast.generic.DebugNode
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, TypeMapping, VisitorHelper}
import vct.col.ast.util.VisitorHelper

/** A single type entry of a record type (only used in `RecordType`). */
case class RecordTypeEntry(val fieldName:String, val fieldType:Type) extends DebugNode {
  override def debugTreeChildrenFields: Iterable[String] = Seq("fieldType")
  override def debugTreePropertyFields: Iterable[String] = Seq("fieldName")
}

/**
 * AST node that represents record types, that is, pairs of variable names (identifiers)
 * and their types.
 * 
 * @todo I think `types` should be an immutable map
 */
case class RecordType(val types:List[RecordTypeEntry]) extends Type with VisitorHelper {
  require(types != null, "The record type is null")
  require(!types.isEmpty, "Record types must have at least one field entry.")
  
  /** Instantiates a record type out of separate lists of `names` and `types`. */
  def this(names:List[String], types:List[Type]) = this((names, types).zipped map (new RecordTypeEntry(_,_)))
  
  /** @return The number of fields in this record. */
  def fieldCount : Int = types.length
  
  /** @return The name of the {@code i}-th record field. */
  def fieldName(i:Int) : String = types.apply(i).fieldName
  
  /** @return The type of the {@code i}-th record field. */
  def fieldType(i:Int) : Type = types.apply(i).fieldType
  
  /** @return Always `false`: no type can extend (inherit from) a record type. */
  override def supertypeof(context:ProgramUnit, t:Type) = false
  
  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))
  override def accept_simple[T](m:TypeMapping[T]) = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields: Iterable[String] = Seq("types", "args")
  override def debugTreePropertyFields: Iterable[String] = Seq()
}
