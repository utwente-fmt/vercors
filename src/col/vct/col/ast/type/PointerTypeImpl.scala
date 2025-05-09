package vct.col.ast.`type`

import vct.col.ast.{PointerType, Type}

trait PointerTypeImpl[G] {
  this: PointerType[G] =>

  val element: Type[G]
  val unique: Option[BigInt]
  val isConst: Boolean
  val isNonNull: Boolean
}
