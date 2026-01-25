package vct.col.ast.`type`

import vct.col.ast.{Expr, PointerArrayType, Type}

trait PointerArrayTypeImpl[G] {
  this: PointerArrayType[G] =>

  val element: Type[G]
  val dimensions: Seq[Option[Expr[G]]]
  val unique: Option[BigInt]
  val isImmutable: Boolean
  val isNonNull: Boolean

  def descend: PointerArrayType[G]
  def asNonNull: Type[G]
  def asNullable: Type[G]
}
