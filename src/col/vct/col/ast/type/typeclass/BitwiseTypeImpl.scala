package vct.col.ast.`type`.typeclass

import vct.col.ast.BitwiseType

trait BitwiseTypeImpl[G] {
  this: BitwiseType[G] =>

  def signed: Boolean
}
