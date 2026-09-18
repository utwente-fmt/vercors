package vct.col.ast.expr.op.cmp

import vct.col.ast.AmbiguousOrderOp
import vct.col.ast.expr.binder.PossibleTriggerImpl

trait AmbiguousOrderOpImpl[G] extends PossibleTriggerImpl[G] {
  this: AmbiguousOrderOp[G] =>

  override def isPossibleTrigger: Boolean = isBagOp || isSetOp
}
