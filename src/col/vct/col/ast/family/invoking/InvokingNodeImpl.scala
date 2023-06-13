package vct.col.ast.family.invoking

import vct.col.ast.{Blame1, ContractApplicable, Expr, InvokingNode, Type, Variable}
import vct.col.check.CheckError
import vct.col.origin.{Blame, InvocationFailure}
import vct.col.ref.Ref

trait InvokingNodeImpl[G] { this: InvokingNode[G] =>
  def ref: Ref[G, _ <: ContractApplicable[G]]
  def blame: Blame1[G]
  def givenMap: Seq[(Ref[G, Variable[G]], Expr[G])]
  def yields: Seq[(Expr[G], Ref[G, Variable[G]])]
  def typeArgs: Seq[Type[G]]
}
