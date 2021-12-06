package vct.col.ast.temporaryimplpackage.expr.apply

import vct.col.ast.{ContractApplicable, Invocation, Type}
import vct.col.origin.{Blame, PreconditionFailed}
import vct.col.ref.Ref

trait InvocationImpl extends ApplyImpl { this: Invocation =>
  override def ref: Ref[_ <: ContractApplicable]
  def blame: Blame[PreconditionFailed]
  def typeArgs: Seq[Type]

  override def t: Type = super.t.particularize(ref.decl.typeArgs.zip(typeArgs).toMap)
}