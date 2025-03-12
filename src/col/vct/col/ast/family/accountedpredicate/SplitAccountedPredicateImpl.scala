package vct.col.ast.family.accountedpredicate

import vct.col.ast.{SplitAccountedPredicate, TBool, TResource, Type}
import vct.col.ast.ops.SplitAccountedPredicateOps

trait SplitAccountedPredicateImpl[G] extends SplitAccountedPredicateOps[G] {
  this: SplitAccountedPredicate[G] =>
  override def t: Type[G] =
    (left.t, right.t) match {
      case (TResource(), _) => TResource()
      case (_, TResource()) => TResource()
      case _ => TBool()
    }
}
