package vct.col.ast.expr.apply

import vct.col.ast.{AbstractPredicate, ApplyAnyPredicate, Expr}
import vct.col.ref.Ref
import vct.col.ast.ops.ApplyAnyPredicateFamilyOps

trait ApplyAnyPredicateImpl[G] extends ApplyAnyPredicateFamilyOps[G] {
  this: ApplyAnyPredicate[G] =>
  def ref: Ref[G, _ <: AbstractPredicate[G]]
  def args: Seq[Expr[G]]
}
