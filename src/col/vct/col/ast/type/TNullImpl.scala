package vct.col.ast.`type`

import vct.col.ast.TNull
import vct.col.ast.ops.TNullOps

trait TNullImpl[G] extends TNullOps[G] {
  this: TNull[G] =>

}
