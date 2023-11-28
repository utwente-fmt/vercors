package vct.col.ast.family.coercion

import vct.col.ast.{CoerceUnboundInt, TInt}
import vct.col.ast.ops.CoerceUnboundIntOps

trait CoerceUnboundIntImpl[G] extends CoerceUnboundIntOps[G] { this: CoerceUnboundInt[G] =>
  override def target: TInt[G] = TInt()
}
