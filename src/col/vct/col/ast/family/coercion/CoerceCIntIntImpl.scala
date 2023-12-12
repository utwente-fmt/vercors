package vct.col.ast.family.coercion

import vct.col.ast.{CoerceCIntInt, TInt}

trait CoerceCIntIntImpl[G] { this: CoerceCIntInt[G] =>
  override def target: TInt[G] = TInt()
}