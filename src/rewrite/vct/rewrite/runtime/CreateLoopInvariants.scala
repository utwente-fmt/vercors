package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers.tt
import vct.rewrite.runtime.util.LedgerHelper._
import vct.rewrite.runtime.util.RewriteContractExpr
import vct.rewrite.runtime.util.Util.findClosestInjectivityMap
import vct.rewrite.runtime.util.permissionTransfer.PermissionData

object CreateLoopInvariants extends RewriterBuilder {
  override def key: String = "createLoopInvariants"

  override def desc: String = "Create loop invariant statements"
}


case class CreateLoopInvariants[Pre <: Generation]() extends Rewriter[Pre] {


  implicit var program: Program[Pre] = _
  implicit var ledger: LedgerMethodBuilderHelper[Post] = _
  val currentClass: ScopedStack[Class[Pre]] = new ScopedStack()
  val loopContract: ScopedStack[() => Statement[Post]] = new ScopedStack()



  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    lazy val newDecl: Seq[GlobalDeclaration[Post]] = globalDeclarations.collect {
      val (ledgerHelper, _, otherDeclarations) = LedgerRewriter[Pre](this).rewriteLedger(program)
      ledger = ledgerHelper
      otherDeclarations.foreach(dispatch)
    }._1
    program.rewrite(declarations = newDecl)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case cls: Class[Pre] => currentClass.having(cls) {
        super.dispatch(cls)
      }
      case _ => super.dispatch(decl)
    }
  }


  def dispatchLoopContract(lc: LoopContract[Pre]): Statement[Post] = {
    val injectivityMap = findClosestInjectivityMap(variables.freeze)
    val pd: PermissionData[Pre] = PermissionData().setOuter(this).setCls(currentClass.top).setLedger(ledger).setInjectivityMap(injectivityMap)
    RewriteContractExpr[Pre](pd).createAssertions(lc.asInstanceOf[LoopInvariant[Pre]].invariant)
  }

  def dispatchLoop(l: Loop[Pre]): Statement[Post] = {
    implicit val o: Origin = l.o
    loopContract.having(() => dispatchLoopContract(l.contract)) {
      val newBody = Block[Post](Seq(loopContract.top(), dispatch(l.body), loopContract.top()))
      l.rewrite(body = newBody, contract = LoopInvariant[Post](tt[Post], None)(null))
    }
  }

  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = {
    stat match {
      case l: Loop[Pre] => dispatchLoop(l)
      case r@(_:Return[Pre] | _:Continue[Pre] | _:Break[Pre] ) if loopContract.nonEmpty => Block[Post](Seq(loopContract.top(), super.dispatch(stat)))(r.o)
      case _ => super.dispatch(stat)
    }
  }

}