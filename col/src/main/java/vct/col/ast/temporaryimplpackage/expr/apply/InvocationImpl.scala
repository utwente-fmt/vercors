package vct.col.ast.temporaryimplpackage.expr.apply

import vct.col.ast.{ContractApplicable, Invocation, Type}
import vct.col.origin.{Blame, PreconditionFailed}
import vct.col.ref.Ref

trait InvocationImpl[G] extends ApplyImpl[G] { this: Invocation[G] =>
  override def ref: Ref[G, _ <: ContractApplicable[G]]
  def blame: Blame[PreconditionFailed]
  def typeArgs: Seq[Type[G]]

  override def t: Type[G] = super.t.particularize(ref.decl.typeArgs.zip(typeArgs).toMap)
}