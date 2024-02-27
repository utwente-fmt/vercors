package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast.{Block, _}
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.rewrite.runtime.util.LedgerHelper.{LedgerMethodBuilderHelper, LedgerRewriter}
import vct.rewrite.runtime.util.Util.findClosestInjectivityMap
import vct.rewrite.runtime.util.{RewriteContractExpr, TransferPermissionRewriter}
import vct.rewrite.runtime.util.permissionTransfer.PermissionData


object CreatePredicateFoldUnfold extends RewriterBuilder {
  override def key: String = "createPredicatesFoldUnfold"
  override def desc: String = "When a fold or unfold predicate is found replace it with a method call to the correct instancePredicate class"
}

case class CreatePredicateFoldUnfold[Pre <: Generation]() extends Rewriter[Pre] {

  implicit var program: Program[Pre] = _
  implicit var ledger: LedgerMethodBuilderHelper[Post] = _
  private val currentClass: ScopedStack[Class[Pre]] = new ScopedStack()


  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case cls: Class[Pre] => currentClass.having(cls) {
        super.dispatch(cls)
      }
      case _ => super.dispatch(decl)
    }
  }

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    lazy val newDecl: Seq[GlobalDeclaration[Post]] = globalDeclarations.collect {
      val (ledgerHelper, _, otherDeclarations) = LedgerRewriter[Pre](this).rewriteLedger(program)
      ledger = ledgerHelper
      otherDeclarations.foreach(dispatch)
    }._1
    val test = program.rewrite(declarations = newDecl)
    test
  }

  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = {
    implicit val origin: Origin = stat.o
    stat match {
      case Unfold(ipa@InstancePredicateApply(_, _, _, _)) => Block[Post](Seq(super.dispatch(stat), dispatchUnfold(ipa)))
      case Fold(ipa@InstancePredicateApply(_, _, _, _)) => Block[Post](Seq(super.dispatch(stat), dispatchFold(ipa)))
      case _ => super.dispatch(stat)
    }
  }

  def dispatchFold(ipa: InstancePredicateApply[Pre])(implicit origin: Origin): Statement[Rewritten[Pre]] = {
    val injectivityMap = findClosestInjectivityMap(variables.freeze)
    val permissionExpr = ipa.ref.decl.body
    variables.collectScoped {
      val newVars = variables.dispatch(ipa.ref.decl.args)
      val newVarsAssignments: Seq[Statement[Post]] = newVars
        .zip(ipa.args)
        .map{case (v: Variable[Post], e: Expr[Pre]) => Assign[Post](v.get, dispatch(e))(null)}
      val nvAssignmentsBlock = Block[Post](newVarsAssignments)

      val pd = PermissionData[Pre]().setOuter(this).setCls(currentClass.top).setLedger(ledger).setInjectivityMap(injectivityMap).setOffset(dispatch(ipa.obj))
      val predCheck: Statement[Post] = if (permissionExpr.isEmpty) Block[Post](Nil) else RewriteContractExpr(pd).createAssertions(permissionExpr.get)
      val removeStatements = if (permissionExpr.isEmpty) Block[Post](Nil) else TransferPermissionRewriter(pd).removePermissions(permissionExpr.get)

      val allArgs: Seq[Expr[Pre]] = ipa.args :+ ipa.obj :+ StringValue(ipa.ref.decl.o.getPreferredNameOrElse())
      val dispatchedArgs: Seq[Expr[Post]] = allArgs.map(dispatch)
      val newObject = CreateObjectArray[Post](dispatchedArgs)
      val mi: Eval[Post] = Eval[Post](ledger.miFoldPredicate(newObject).get)
      Scope[Post](newVars, Block[Post](Seq(nvAssignmentsBlock, predCheck, removeStatements, mi)))
    }._2
  }

  def dispatchUnfold(ipa: InstancePredicateApply[Pre])(implicit origin: Origin): Statement[Rewritten[Pre]] = {
    val permissionExpr = ipa.ref.decl.body
    val pdAdd: PermissionData[Pre] = PermissionData[Pre]().setOuter(this).setCls(currentClass.top)
      .setLedger(ledger).setOffset(dispatch(ipa.obj))
    val addStatements = if(permissionExpr.isEmpty) Block[Post](Nil) else TransferPermissionRewriter(pdAdd).addPermissions(permissionExpr.get)


    val allArgs: Seq[Expr[Pre]] = ipa.args :+ ipa.obj :+ StringValue(ipa.ref.decl.o.getPreferredNameOrElse())
    val dispatchedArgs: Seq[Expr[Post]] = allArgs.map(dispatch)
    val newObject = CreateObjectArray[Post](dispatchedArgs)
    val mi: Eval[Post] = Eval[Post](ledger.miUnfoldPredicate(newObject).get)
    Block[Post](Seq(mi, addStatements))
  }

}