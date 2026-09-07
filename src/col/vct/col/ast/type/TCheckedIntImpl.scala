package vct.col.ast.`type`

import vct.col.ast.TCheckedInt
import vct.col.ast.ops.TCheckedIntOps

trait TCheckedIntImpl[G] extends TCheckedIntOps[G] {
  this: TCheckedInt[G] =>

}
