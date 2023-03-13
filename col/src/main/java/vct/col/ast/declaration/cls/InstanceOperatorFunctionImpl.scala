package vct.col.ast.declaration.cls

import vct.col.ast.{InstanceOperatorFunction, Variable}

trait InstanceOperatorFunctionImpl[G] { this: InstanceOperatorFunction[G] =>
  def typeArgs: Seq[Variable[G]] = Nil
}
