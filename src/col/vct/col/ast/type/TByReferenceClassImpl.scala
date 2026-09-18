package vct.col.ast.`type`

import vct.col.ast.TByReferenceClass
import vct.col.ast.ops.TByReferenceClassOps

trait TByReferenceClassImpl[G] extends TByReferenceClassOps[G] {
  this: TByReferenceClass[G] =>
}
