package vct.col.ast.declaration.cls

import vct.col.ast.{InstanceOperatorMethod, Type, Variable}

trait InstanceOperatorMethodImpl[G] { this: InstanceOperatorMethod[G] =>
  def typeArgs: Seq[Variable[G]] = Nil
  def outArgs: Seq[Variable[G]] = Nil
}
