package vct.col.ast.family.coercion

import vct.col.ast.{CoerceCheckedIntInt, TInt}
import vct.col.ast.ops.CoerceCheckedIntIntOps

trait CoerceCheckedIntIntImpl[G] extends CoerceCheckedIntIntOps[G] {
  this: CoerceCheckedIntInt[G] =>
  override def target: TInt[G] = TInt()
}
