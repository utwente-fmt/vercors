package vct.col.ast.family.coercion

import vct.col.ast.CoercePointerNonNull
import vct.col.ast.ops.CoercePointerNonNullOps

trait CoercePointerNonNullImpl[G] extends CoercePointerNonNullOps[G] {
  this: CoercePointerNonNull[G] =>
}
