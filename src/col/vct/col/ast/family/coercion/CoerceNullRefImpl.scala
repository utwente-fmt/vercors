package vct.col.ast.family.coercion

import vct.col.ast.{CoerceNullRef, TRef}
import vct.col.ast.ops.CoerceNullRefOps

trait CoerceNullRefImpl[G] extends CoerceNullRefOps[G] { this: CoerceNullRef[G] => 
  override def target: TRef[G] = TRef()
}
