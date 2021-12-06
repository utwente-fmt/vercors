package vct.col.ast.temporaryimplpackage.expr.apply

import vct.col.ast.{AbstractMethod, AnyMethodInvocation}
import vct.col.ref.Ref

trait AnyMethodInvocationImpl { this: AnyMethodInvocation =>
  override def ref: Ref[_ <: AbstractMethod]
}