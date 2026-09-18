package vct.col.ast.family.coercion

import vct.col.ast.ops.CoerceNullPointerArrayOps
import vct.col.ast.CoerceNullPointerArray

trait CoerceNullPointerArrayImpl[G] extends CoerceNullPointerArrayOps[G] {
  this: CoerceNullPointerArray[G] =>
}
