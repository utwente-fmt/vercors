package vct.col.ast.`type`

import vct.col.ast.TAny
import vct.col.ast.ops.TAnyOps

trait TAnyImpl[G] extends TAnyOps[G] {
  this: TAny[G] =>

}
