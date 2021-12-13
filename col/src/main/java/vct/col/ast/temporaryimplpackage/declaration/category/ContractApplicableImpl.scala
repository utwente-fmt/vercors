package vct.col.ast.temporaryimplpackage.declaration.category

import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.ast.{ApplicableContract, ContractApplicable, Declaration, Variable}
import vct.col.origin.{Blame, PostconditionFailed}

trait ContractApplicableImpl[G] extends InlineableApplicableImpl[G] { this: ContractApplicable[G] =>
  def contract: ApplicableContract[G]
  def blame: Blame[PostconditionFailed]
  override def declarations: Seq[Declaration[G]] =
    super.declarations ++ contract.givenArgs ++ contract.yieldsArgs ++ typeArgs

  // PB: Not necessarily the logical place to introduce type arguments, but it happens to be correct: as many places
  // as possible, but not predicates (for now), and ADT functions are dealt with in a special way: they inherit the
  // type parameters from the ADT itself.
  def typeArgs: Seq[Variable[G]]
}
