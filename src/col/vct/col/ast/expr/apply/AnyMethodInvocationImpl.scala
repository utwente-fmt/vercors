package vct.col.ast.expr.apply

import vct.col.ast.{AbstractMethod, AnyMethodInvocation, Expr}
import vct.col.ref.Ref

trait AnyMethodInvocationImpl[G] { this: AnyMethodInvocation[G] =>
  override def ref: Ref[G, _ <: AbstractMethod[G]]
  def outArgs: Seq[Expr[G]]
}