package vct.col.ast.family.coercion

import vct.col.ast.CoerceNothingSomething
import vct.col.ast.ops.CoerceNothingSomethingOps

trait CoerceNothingSomethingImpl[G] extends CoerceNothingSomethingOps[G] {
  this: CoerceNothingSomething[G] =>

}
