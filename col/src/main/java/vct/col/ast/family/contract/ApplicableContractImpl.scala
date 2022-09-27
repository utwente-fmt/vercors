package vct.col.ast.family.contract

import vct.col.ast.{ApplicableContract, BooleanValue, UnitAccountedPredicate}
import vct.col.ast.node.NodeFamilyImpl
import vct.col.check.{CheckContext, CheckError}

trait ApplicableContractImpl[G] extends NodeFamilyImpl[G] { this: ApplicableContract[G] =>
  override def checkTrans(context: CheckContext[G]): Seq[CheckError] =
    this match {
      // Redundant match so this doesn't compile if we add a field to ApplicableContract
      case ApplicableContract(requires, ensures, contextEverywhere, signals, givenArgs, yieldsArgs, decreases) =>
        val childErrors =
          requires.checkTrans(context) ++ ensures.checkTrans(context.withPostcondition) ++ contextEverywhere.checkTrans(context) ++
            signals.flatMap(_.checkTrans(context)) ++ givenArgs.flatMap(_.checkTrans(context)) ++ yieldsArgs.flatMap(_.checkTrans(context)) ++
            decreases.toSeq.flatMap(_.checkTrans(context))

        childErrors match {
          case Nil => check(context)
          case some => some
        }
    }

  def isEmpty: Boolean = this match {
    case ApplicableContract(UnitAccountedPredicate(BooleanValue(true)), UnitAccountedPredicate(BooleanValue(true)), BooleanValue(true), Nil, Nil, Nil, None) => true
    case _ => false
  }

  def nonEmpty: Boolean = !isEmpty
}