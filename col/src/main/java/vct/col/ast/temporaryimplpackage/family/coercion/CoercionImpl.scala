package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{Coercion, Type}

trait CoercionImpl[G] { this: Coercion[G] =>
  def target: Type[G]
}
