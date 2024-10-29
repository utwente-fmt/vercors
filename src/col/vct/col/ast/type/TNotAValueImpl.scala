package vct.col.ast.`type`

import vct.col.ast.TNotAValue
import vct.col.ast.ops.TNotAValueOps

trait TNotAValueImpl[G] extends TNotAValueOps[G] {
  this: TNotAValue[G] =>

}
