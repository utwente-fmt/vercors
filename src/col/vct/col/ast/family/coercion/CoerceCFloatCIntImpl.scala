package vct.col.ast.family.coercion

import vct.col.ast.{CoerceCFloatCInt, TInt}

trait CoerceCFloatCIntImpl[G] { this: CoerceCFloatCInt[G] =>
  override def target: TInt[G] = TInt()
}
