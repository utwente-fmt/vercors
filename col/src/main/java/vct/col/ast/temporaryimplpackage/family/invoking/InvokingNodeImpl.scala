package vct.col.ast.temporaryimplpackage.family.invoking

import vct.col.ast.{ContractApplicable, InvokingNode, Type}
import vct.col.origin.{Blame, InvocationFailure}
import vct.col.ref.Ref

trait InvokingNodeImpl[G] { this: InvokingNode[G] =>
  def ref: Ref[G, _ <: ContractApplicable[G]]
  def blame: Blame[InvocationFailure]
  def typeArgs: Seq[Type[G]]
}
