package vct.col.ast.`type`

import vct.col.ast.TByValueClass
import vct.col.ast.ops.TByValueClassOps

trait TByValueClassImpl[G] extends TByValueClassOps[G] {
  this: TByValueClass[G] =>
}
