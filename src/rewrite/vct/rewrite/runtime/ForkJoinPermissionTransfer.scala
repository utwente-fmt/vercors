package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.LedgerHelper._
import vct.rewrite.runtime.util.TransferPermissionRewriter
import vct.rewrite.runtime.util.Util._
import vct.rewrite.runtime.util.permissionTransfer.PermissionData

import scala.collection.mutable
import scala.language.postfixOps


object ForkJoinPermissionTransfer extends RewriterBuilder {
  override def key: String = "forkJoinPermissionTransfer"

  override def desc: String = "Detects fork/join/run methods and creates a permission transfer for the forked thread"
}

case class ForkJoinPermissionTransfer[Pre <: Generation]() extends Rewriter[Pre] {

  implicit var program: Program[Pre] = _
  implicit var ledger: LedgerMethodBuilderHelper[Post] = _

  private val currentClass: ScopedStack[Class[Pre]] = new ScopedStack()
  private val postJoinTokens: ScopedStack[mutable.ArrayBuffer[RuntimePostJoin[Post]]] = new ScopedStack()

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
      case i: InstanceMethod[Pre] => dispatchInstanceMethod(i)
      case _ => super.dispatch(decl)
    }
  }

  /**
   * Dispatches the instance method to the method with transferring permission statements
   * @param i
   * @param o
   */
  private def dispatchInstanceMethod(i: InstanceMethod[Pre])(implicit o: Origin = i.o): Unit = {
    postJoinTokens.collect {
      variables.collectScoped {
        val (preStatements, postStatements): (Block[Post], Block[Post]) = collectTransferPermissionStatementsFromRunMethod(i)
        val scope = collectMethodBody(i)
        val scopeBlock = collectBlockScope(scope)
        val newScope = scope.rewrite(body = Block[Post](Seq(preStatements, dispatch(scopeBlock), postStatements)))
        classDeclarations.succeed(i, i.rewrite(body = Some(newScope)))
      }
    }
  }

  /**
   * Collects the transfer permission statements from the run method
   * where the pre and post conditions will be used to add and remove permissions from the thread
   * @param i
   * @param o
   * @return
   */
  private def collectTransferPermissionStatementsFromRunMethod(i: InstanceMethod[Pre]): (Block[Post], Block[Post]) = {
    implicit val o: Origin = i.o
    if (!isExtendingThread(currentClass.top) || !isMethod(i, "run")) return (EMPTY, EMPTY)
    val pd: PermissionData[Pre] = PermissionData[Pre]().setOuter(this).setCls(currentClass.top).setLedger(ledger)
    val trans = TransferPermissionRewriter(pd)
    val (prePredicate, postPredicate) = (unfoldPredicate(i.contract.requires).head, unfoldPredicate(i.contract.ensures).head)
    (trans.addPermissions(prePredicate), trans.removePermissions(postPredicate))
  }

  /**
   * When a post join token is found, store the token in the buffer
   * When an join or start methodinvocation is found dispatch the method invocation to the also transfer permissions
   * @param stat
   * @return
   */
  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = {
    stat match {
      case rpj: RuntimePostJoin[Pre] if postJoinTokens.nonEmpty => {
        val newRpj = super.dispatch(rpj)
        postJoinTokens.top.addOne(newRpj.asInstanceOf[RuntimePostJoin[Post]])
        newRpj
      }
      case e@Eval(mi: MethodInvocation[Pre]) if isThreadMethod(mi, "join") => dispatchJoinInvocation(e, mi)
      case e@Eval(mi: MethodInvocation[Pre]) if isThreadMethod(mi, "start") => dispatchStartInvocation(e, mi)
      case _ => super.dispatch(stat)
    }
  }

  /**
   * Dispatches the join method invocation to the method with adding permission statements for the joined thread
   * Using the join token as a factor to remove the permissions from the thread
   * @param e
   * @param mi
   * @param o
   * @return
   */
  private def dispatchJoinInvocation(e: Eval[Pre], mi: MethodInvocation[Pre])(implicit o: Origin = e.o): Statement[Rewritten[Pre]] = {
    val runMethod: InstanceMethod[Pre] = getRunMethod(mi)
    val predicate: Expr[Pre] = unfoldPredicate(runMethod.contract.ensures).head
    val dispatchedStatement: Eval[Post] = super.dispatch(e).asInstanceOf[Eval[Post]]
    val dispatchedOffset: Expr[Post] = dispatchedStatement.expr.asInstanceOf[MethodInvocation[Post]].obj
    val pst = postJoinTokens.top
    val postfactor: Expr[Post] = postJoinTokens.top
      .find(rpj => rpj.obj == dispatchedOffset)
      .get.arg

    val factor = permissionToRuntimeValue(postfactor)
    val removePostJoinToken: Eval[Post] = Eval[Post](ledger.miSetJoinToken(dispatchedOffset, ledger.miGetJoinToken(dispatchedOffset).get r_- factor).get)
    val pdAdd: PermissionData[Pre] = PermissionData[Pre]().setOuter(this).setCls(currentClass.top).setLedger(ledger).setOffset(dispatch(mi.obj)).setFactor(factor)
    val newAddStatements = TransferPermissionRewriter(pdAdd).addPermissions(predicate)
    Block[Post](Seq(dispatchedStatement, removePostJoinToken, newAddStatements))
  }

  /**
   * Dispatches the start method invocation to the method with removing permission statements of the starting thread
   * @param e
   * @param mi
   * @param o
   * @return
   */
  private def dispatchStartInvocation(e: Eval[Pre], mi: MethodInvocation[Pre])(implicit o: Origin = e.o): Statement[Rewritten[Pre]] = {
    val runMethod: InstanceMethod[Pre] = getRunMethod(mi)
    val predicate: Expr[Pre] = unfoldPredicate(runMethod.contract.requires).head
    val dispatchedStatement: Eval[Post] = super.dispatch(e).asInstanceOf[Eval[Post]]
    val pdRemove: PermissionData[Pre] = PermissionData[Pre]().setOuter(this).setCls(currentClass.top).setLedger(ledger).setOffset(dispatch(mi.obj))
    val removeStatements = TransferPermissionRewriter(pdRemove).removePermissions(predicate)
    Block[Post](Seq(removeStatements, dispatchedStatement))
  }
}