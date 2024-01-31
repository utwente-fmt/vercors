package vct.rewrite.runtime

import vct.col.ast.RewriteHelpers._
import vct.col.ast.{Block, _}
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.rewrite.runtime.util.LedgerHelper.{LedgerMethodBuilderHelper, LedgerRewriter}
import vct.rewrite.runtime.util.Util.{InstancePredicateData, findInstancePredicateData}


object CreatePredicateFoldUnfold extends RewriterBuilder {
  override def key: String = "createPredicatesFoldUnfold"
  override def desc: String = "When a fold or unfold predicate is found replace it with a method call to the correct instancePredicate class"
}




case class CreatePredicateFoldUnfold[Pre <: Generation]() extends Rewriter[Pre] {


  implicit var program: Program[Pre] = _
  implicit var ledger: LedgerMethodBuilderHelper[Post] = _


  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    lazy val newDecl: Seq[GlobalDeclaration[Post]] = globalDeclarations.collect {
      val (ledgerHelper, ledgerClass, otherDeclarations) = LedgerRewriter[Pre](this).rewriteLedger(program)
      ledger = ledgerHelper
      otherDeclarations.foreach(dispatch)
    }._1
    program.rewrite(declarations = newDecl)
  }

  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = {
    implicit val origin: Origin = stat.o
    stat match {
      case Unfold(ipa@InstancePredicateApply(_, _, _, _)) => Block[Post](Seq(super.dispatch(stat), dispatchFoldUnfold(ipa, CreatePredicates.UNFOLD)))
      case Fold(ipa@InstancePredicateApply(_, _, _, _)) => Block[Post](Seq(super.dispatch(stat), dispatchFoldUnfold(ipa, CreatePredicates.FOLD)))
      case _ => super.dispatch(stat)
    }
  }

  def dispatchFoldUnfold(ipa: InstancePredicateApply[Pre] , methodName: String)(implicit origin: Origin) : Statement[Post] = {
    val ipd: InstancePredicateData[Pre] = findInstancePredicateData(ipa)
    val unfoldPredicateMI = ipd.createMethodInvocation(methodName)
    Eval[Post](super.dispatch(unfoldPredicateMI))
  }
}