package vct.col.ast.family.coercion

import vct.col.ast.ops.CoerceNonNullPointerArrayOps
import vct.col.ast.CoerceNonNullPointerArray

trait CoerceNonNullPointerArrayImpl[G] extends CoerceNonNullPointerArrayOps[G] {
  this: CoerceNonNullPointerArray[G] =>
}
