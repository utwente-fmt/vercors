package vct.col.ast.family.coercion

import vct.col.ast.CoerceBoundIntFloat
import vct.col.ast.ops.CoerceBoundIntFloatOps

trait CoerceBoundIntFloatImpl[G] extends CoerceBoundIntFloatOps[G] {
  this: CoerceBoundIntFloat[G] =>
}
