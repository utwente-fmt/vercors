package vct.col.ast.family.invoking

import vct.col.ast.{ContractApplicable, Expr, InvokingNode, Type, Variable}
import vct.col.check.CheckError
import vct.col.origin.{Blame, InvocationFailure}
import vct.col.ref.Ref

trait InvokingNodeImpl[G] { this: InvokingNode[G] =>
  def ref: Ref[G, _ <: ContractApplicable[G]]
  def blame: Blame[InvocationFailure]
  def givenMap: Seq[(Ref[G, Variable[G]], Expr[G])]
  def yields: Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])]
  def typeArgs: Seq[Type[G]]
}
