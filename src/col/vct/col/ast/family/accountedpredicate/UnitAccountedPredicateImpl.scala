package vct.col.ast.family.accountedpredicate

import vct.col.ast.{TResource, UnitAccountedPredicate}
import vct.col.ast.ops.UnitAccountedPredicateOps
import vct.col.check._
import vct.col.typerules.{CoercionUtils}

trait UnitAccountedPredicateImpl[G] extends UnitAccountedPredicateOps[G] {
  this: UnitAccountedPredicate[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] =
    CoercionUtils.getCoercion(pred.t, TResource()) match {
      case Some(_) => Nil
      case None => Seq(TypeError(pred, TResource()))
    }

}
