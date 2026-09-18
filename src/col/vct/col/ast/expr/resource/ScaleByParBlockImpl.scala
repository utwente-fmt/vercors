package vct.col.ast.expr.resource

import vct.col.ast.expr.binder.PossibleTriggerImpl
import vct.col.ast.{PossibleTrigger, ScaleByParBlock, Type}
import vct.col.ast.ops.ScaleByParBlockOps

trait ScaleByParBlockImpl[G]
    extends ScaleByParBlockOps[G] with PossibleTriggerImpl[G] {
  this: ScaleByParBlock[G] =>
  override def t: Type[G] = res.t

  override def isPossibleTrigger: Boolean =
    res match {
      case t: PossibleTrigger[G] => t.isPossibleTrigger
      case _ => false
    }
}
