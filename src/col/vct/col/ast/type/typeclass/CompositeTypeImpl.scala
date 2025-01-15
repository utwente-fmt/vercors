package vct.col.ast.`type`.typeclass

import vct.col.ast.{CompositeType, Type}

trait CompositeTypeImpl[G] {
  this: CompositeType[G] =>
  def composingTypes: Seq[Type[G]]
}
