package vct.col.ast.family.invoking

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.{ContractApplicable, Expr, InvokingNode, Type, Variable}
import vct.col.check.{CheckContext, CheckError}
import vct.col.origin.{Blame, InvocationFailure, PreBlameSplit}
import vct.col.ref.Ref

trait InvokingNodeImpl[G] extends NodeFamilyImpl[G] {
  this: InvokingNode[G] =>
  def ref: Ref[G, _ <: ContractApplicable[G]]
  def blame: Blame[InvocationFailure]
  def givenMap: Seq[(Ref[G, Variable[G]], Expr[G])]
  def yields: Seq[(Expr[G], Ref[G, Variable[G]])]
  def typeArgs: Seq[Type[G]]

  override def check(context: CheckContext[G]): Seq[CheckError] = {
    blame match {
      case pbs: PreBlameSplit[_] =>
        pbs.checkConsistency(ref.decl.contract.requires)
      case _ =>
    }
    super.check(context)
  }
}
