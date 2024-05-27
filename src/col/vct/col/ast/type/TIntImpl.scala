package vct.col.ast.`type`

import vct.col.ast.TInt
import vct.col.ast.ops.TIntOps

trait TIntImpl[G] extends TIntOps[G] {
  this: TInt[G] =>

}
