package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.{RewriteInstanceMethod, RewriteScope}
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.{PermissionRewriter, TransferPermissionRewriter}
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
  private val currentClass: ScopedStack[Class[Pre]] = new ScopedStack()
  private val postJoinTokens: ScopedStack[mutable.ArrayBuffer[RuntimePostJoin[Post]]] = new ScopedStack()

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    val test = super.dispatch(program)
    test
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

  protected def dispatchInstanceMethod(i: InstanceMethod[Pre])(implicit o: Origin = i.o): Unit = {
    postJoinTokens.collect {
      variables.collectScoped {
        val transferPermissionsStatements: Block[Post] = collectTransferPermissionStatementsFromRunMethod(i)
        val scope = collectMethodBody(i)
        val scopeBlock = collectBlockScope(scope)
        val newScope = scope.rewrite(body = Block[Post](Seq(transferPermissionsStatements, dispatch(scopeBlock))))
        classDeclarations.succeed(i, i.rewrite(body = Some(newScope)))
      }
    }
  }

  private def collectTransferPermissionStatementsFromRunMethod(i: InstanceMethod[Pre]): Block[Post] = {
    if (!isExtendingThread(currentClass.top) || !isMethod(i, "run")) return Block[Post](Seq.empty)(i.o)
    val predicate = unfoldPredicate(i.contract.requires).head
    val pd: PermissionData[Pre] = PermissionData[Pre]().setOuter(this).setCls(currentClass.top)
    TransferPermissionRewriter(pd).addPermissions(predicate)
  }

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

  private def dispatchJoinInvocation(e: Eval[Pre], mi: MethodInvocation[Pre])(implicit o: Origin = e.o): Statement[Rewritten[Pre]] = {
    val runMethod: InstanceMethod[Pre] = getRunMethod(mi)
    val predicate: Expr[Pre] = unfoldPredicate(runMethod.contract.ensures).head
    val dispatchedStatement: Eval[Post] = super.dispatch(e).asInstanceOf[Eval[Post]]
    val dispatchedOffset: Expr[Post] = getDispatchedOffset(dispatchedStatement)
    val postfactor: Expr[Post] = postJoinTokens.top.find(rpj => rpj.obj == dispatchedOffset).get.arg
    val factor = PermissionRewriter.permissionToRuntimeValue(postfactor)
    val pdAdd: PermissionData[Pre] = PermissionData[Pre]().setOuter(this).setCls(currentClass.top).setFactor(factor)
    val newAddStatements = TransferPermissionRewriter(pdAdd).addPermissions(predicate)
    val pdRemove: PermissionData[Pre] = PermissionData[Pre]().setOuter(this).setCls(currentClass.top).setFactor(factor).setOffset(dispatchedOffset).setThreadId(dispatchedOffset)
    val removeStatements = TransferPermissionRewriter(pdRemove).removePermissions(predicate)
//    val removeStatements = TransferPermissionRewriter(this, currentClass.top, (Some(mi.obj), None),(Some(mi.obj), None), factor, Seq(mi.obj.asInstanceOf[Local[Pre]].ref.decl)).removePermissions(predicate)
    Block[Post](Seq(dispatchedStatement, newAddStatements, removeStatements))
  }

  private def getDispatchedOffset(e: Eval[Post]): Expr[Post] = {
    e.expr match {
      case mi: MethodInvocation[Post] => mi.obj
      case _ => throw Unreachable("Only method invocations are expected")
    }
  }

  private def dispatchStartInvocation(e: Eval[Pre], mi: MethodInvocation[Pre])(implicit o: Origin = e.o): Statement[Rewritten[Pre]] = {
    val runMethod: InstanceMethod[Pre] = getRunMethod(mi)
    val predicate: Expr[Pre] = unfoldPredicate(runMethod.contract.requires).head
    val dispatchedStatement: Eval[Post] = super.dispatch(e).asInstanceOf[Eval[Post]]
    val pdRemove: PermissionData[Pre] = PermissionData[Pre]().setOuter(this).setCls(currentClass.top)
    val removeStatements = TransferPermissionRewriter(pdRemove).removePermissions(predicate)
    Block[Post](Seq(removeStatements, dispatchedStatement))
  }
}