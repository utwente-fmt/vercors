package vct.col.ast.`type`

import vct.col.ast.TBoundedInt
import vct.col.ast.ops.TBoundedIntOps

trait TBoundedIntImpl[G] extends TBoundedIntOps[G] {
  this: TBoundedInt[G] =>

}
