package vct.col.ast.temporaryimplpackage.expr.apply

import vct.col.ast.{AbstractPredicate, ApplyAnyPredicate}
import vct.col.ref.Ref

trait ApplyAnyPredicateImpl { this: ApplyAnyPredicate =>
  override def ref: Ref[_ <: AbstractPredicate]
}