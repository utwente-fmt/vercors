package vct.col.ast.family.coercion

import vct.col.ast.{CoerceCIntInt, TInt}
import vct.col.ast.ops.CoerceCIntIntOps

trait CoerceCIntIntImpl[G] extends CoerceCIntIntOps[G] {
  this: CoerceCIntInt[G] =>
  override def target: TInt[G] = TInt()
}
