package vct.col.ast.expr.apply

import vct.col.ast.{Invocation, Type, Variable}

trait InvocationImpl[G] extends ApplyImpl[G] { this: Invocation[G] =>
  override def t: Type[G] = super.t.particularize(typeEnv)
  def typeEnv: Map[Variable[G], Type[G]]
}