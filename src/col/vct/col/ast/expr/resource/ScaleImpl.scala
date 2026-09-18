package vct.col.ast.expr.resource

import vct.col.ast.expr.binder.PossibleTriggerImpl
import vct.col.ast.{PossibleTrigger, Scale, TResource, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.ScaleOps

trait ScaleImpl[G] extends ScaleOps[G] with PossibleTriggerImpl[G] {
  this: Scale[G] =>
  override def t: Type[G] = TResource()

  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("[") <> scale <> "]" <> assoc(res)

  override def isPossibleTrigger: Boolean =
    res match {
      case t: PossibleTrigger[G] => t.isPossibleTrigger
      case _ => false
    }
}
