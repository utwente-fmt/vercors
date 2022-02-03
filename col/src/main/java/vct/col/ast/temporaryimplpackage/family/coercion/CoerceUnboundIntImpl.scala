package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceUnboundInt, TInt}

trait CoerceUnboundIntImpl[G] { this: CoerceUnboundInt[G] =>
  override def target: TInt[G] = TInt()
}
