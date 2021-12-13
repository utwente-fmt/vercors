package vct.col.ast.temporaryimplpackage.expr.apply

import vct.col.ast.{AbstractMethod, AnyMethodInvocation}
import vct.col.ref.Ref

trait AnyMethodInvocationImpl[G] { this: AnyMethodInvocation[G] =>
  override def ref: Ref[G, _ <: AbstractMethod[G]]
}