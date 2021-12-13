package vct.col.ast.temporaryimplpackage.expr.apply

import vct.col.ast.{AbstractPredicate, ApplyAnyPredicate}
import vct.col.ref.Ref

trait ApplyAnyPredicateImpl[G] { this: ApplyAnyPredicate[G] =>
  override def ref: Ref[G, _ <: AbstractPredicate[G]]
}