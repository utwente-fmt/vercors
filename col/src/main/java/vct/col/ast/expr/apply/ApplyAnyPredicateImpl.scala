package vct.col.ast.expr.apply

import vct.col.ast.{AbstractPredicate, ApplyAnyPredicate, Expr}
import vct.col.ref.Ref

trait ApplyAnyPredicateImpl[G] { this: ApplyAnyPredicate[G] =>
  override def ref: Ref[G, _ <: AbstractPredicate[G]]
  def perm: Expr[G]
}