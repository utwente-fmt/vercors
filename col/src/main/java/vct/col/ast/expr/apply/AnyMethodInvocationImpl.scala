package vct.col.ast.expr.apply

import vct.col.ast.{AbstractMethod, AnyMethodInvocation, Variable}
import vct.col.ref.Ref

trait AnyMethodInvocationImpl[G] { this: AnyMethodInvocation[G] =>
  override def ref: Ref[G, _ <: AbstractMethod[G]]
  def outArgs: Seq[Ref[G, Variable[G]]]
}