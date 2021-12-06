package vct.col.ast.temporaryimplpackage.expr.apply

import vct.col.ast.{AbstractFunction, AnyFunctionInvocation}
import vct.col.ref.Ref

trait AnyFunctionInvocationImpl { this: AnyFunctionInvocation =>
  override def ref: Ref[_ <: AbstractFunction]
}