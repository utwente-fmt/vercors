package vct.col.ast.temporaryimplpackage.expr.apply

import vct.col.ast.{AbstractFunction, AnyFunctionInvocation}
import vct.col.ref.Ref

trait AnyFunctionInvocationImpl[G] { this: AnyFunctionInvocation[G] =>
  override def ref: Ref[G, _ <: AbstractFunction[G]]
}