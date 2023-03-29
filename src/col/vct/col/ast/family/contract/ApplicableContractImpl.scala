package vct.col.ast.family.contract

import vct.col.ast.{ApplicableContract, BooleanValue, Node, UnitAccountedPredicate}
import vct.col.ast.node.NodeFamilyImpl
import vct.col.check.{CheckContext, CheckError}

trait ApplicableContractImpl[G] extends NodeFamilyImpl[G] { this: ApplicableContract[G] =>
  override def checkContextRecursor[T](context: CheckContext[G], f: (CheckContext[G], Node[G]) => T): Seq[T] =
    this match {
      // Redundant match so this doesn't compile if we add a field to ApplicableContract
      case ApplicableContract(requires, ensures, contextEverywhere, signals, givenArgs, yieldsArgs, decreases) =>
        f(context, requires) +: f(context.withPostcondition, ensures) +: f(context, contextEverywhere) +:
          (signals.map(f(context, _)) ++ givenArgs.map(f(context, _)) ++ yieldsArgs.map(f(context, _)) ++
          decreases.toSeq.map(f(context, _)))
    }

  def isEmpty: Boolean = this match {
    case ApplicableContract(UnitAccountedPredicate(BooleanValue(true)), UnitAccountedPredicate(BooleanValue(true)), BooleanValue(true), Nil, Nil, Nil, None) => true
    case _ => false
  }

  def nonEmpty: Boolean = !isEmpty
}