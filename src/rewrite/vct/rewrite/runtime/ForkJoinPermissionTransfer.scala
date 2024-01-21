package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.{RewriteInstanceMethod, RewriteScope, RewriteVariable}
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.Util._
import vct.rewrite.runtime.util.TransferPermissionRewriter

import scala.collection.mutable


object ForkJoinPermissionTransfer extends RewriterBuilder {
  override def key: String = "forkJoinPermissionTransfer"

  override def desc: String = "Detects fork/join/run methods and creates a permission transfer for the forked thread"
}

case class ForkJoinPermissionTransfer[Pre <: Generation]() extends Rewriter[Pre] {

  implicit var program: Program[Pre] = null

  val currentClass: ScopedStack[Class[Pre]] = new ScopedStack()
  val postJoinTokens: ScopedStack[mutable.ArrayBuffer[RuntimePostJoin[Post]]] = new ScopedStack()

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    val test = super.dispatch(program)
    test
  }



//  def dispatchMethodStart(mi: MethodInvocation[Pre]): Expr[Post] = {
//    implicit val o: Origin = mi.o
//    val (predicate, offset, variable) = dispatchMethodInvocation(mi, "start")
//    //    implicit val firstRequiredLocals: Seq[Variable[Pre]] = variable
//    //TODO check that the thread has enough permissions to do this operation
//    //    doPermissionTransfer(predicate, offset = offset, firstLocals = variable)
//    val newStatements = TransferPermissionRewriter(this, currentClass.top, None, None).removePermissions(predicate)
//    statementBuffer.top.addAll(newStatements)
//    super.dispatch(mi)
//  }
//
//  def dispatchMethodJoin(mi: MethodInvocation[Pre]): Expr[Post] = {
//    implicit val o: Origin = mi.o
//    val (predicate, offset, variable) = dispatchMethodInvocation(mi, "join")
//    val dispatchedOffset = dispatch(offset.get)
//    val pjts = postJoinTokens.top.toSeq
//    val factor: Option[Expr[Post]] = Some(postJoinTokens.top.find(rpj => rpj.obj == dispatchedOffset).get.arg)
//    implicit val firstRequiredLocals: Seq[Variable[Pre]] = variable
//    val newAddStatements = TransferPermissionRewriter(this, currentClass.top, None, factor).addPermissions(predicate)
//    val removeStatements = TransferPermissionRewriter(this, currentClass.top, offset, factor).removePermissions(predicate)
//    statementBuffer.top.addAll(removeStatements)
//    statementBuffer.top.addAll(newAddStatements)
//    //    doPermissionTransfer(predicate, offset = offset, factor = factor)
//    //    doPermissionTransfer(predicate, offset = offset, factor = factor, firstLocals = variable)
//    //TODO check that the joining thread has enough permissions to remove the permissions
//    super.dispatch(mi)
//  }


  def collectTransferPermissionStatementsFromRunMethod(i: InstanceMethod[Pre]) : Seq[Statement[Post]] = {
    if (!isExtendingThread(currentClass.top) || !isMethod(i, "run")) return Seq.empty
    implicit val o: Origin = i.o
    val predicate = unfoldPredicate(i.contract.requires).head
    TransferPermissionRewriter(this, currentClass.top, None, None, Seq.empty).addPermissions(predicate)
  }

  protected def dispatchInstanceMethod(i: InstanceMethod[Pre])(implicit o: Origin = i.o): Unit = {
    postJoinTokens.collect {
      variables.collectScoped {
        val transferPermissionsStatements: Seq[Statement[Post]] = collectTransferPermissionStatementsFromRunMethod(i)
        val scope = collectMethodBody(i)
        val scopeBlock = collectBlockScope(scope)
        val newScope = scope.rewrite(body = Block[Post](transferPermissionsStatements :+ dispatch(scopeBlock)))
        classDeclarations.succeed(i, i.rewrite(body = Some(newScope)))
      }
    }
  }

//    protected def dispatchInstanceMethod(i: InstanceMethod[Pre], transferPermissionsStatements: Seq[Statement[Post]] = Seq.empty): Unit = {
//      implicit val o: Origin = i.o
//
//      postJoinTokens.collect {
//        variables.collectScoped {
//          val newArgs = i.args.map(v => variables.succeed(v, v.rewrite()))
//          val newContract = dispatch(i.contract)
//          i.body match {
//            case Some(sc: Scope[Pre]) => sc.body match {
//              case b: Block[Pre] => {
//                val newLocals = sc.locals.map(v => variables.succeed(v, v.rewrite()))
//                val newScope = RewriteScope(sc)(this).rewrite(locals = newLocals, body = dispatchBlock(b, transferPermissionsStatements))
//                classDeclarations.succeed(i, i.rewrite(args = newArgs, contract = newContract, body = Some(newScope)))
//              }
//            }
//          }
//        }
//      }
//    }

//  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
//    e match {
//      case mi: MethodInvocation[Pre] if isThreadMethod(mi, "join") => dispatchMethodJoin(mi)
//      case mi: MethodInvocation[Pre] if isThreadMethod(mi, "start") => dispatchMethodStart(mi)
//      case _ => super.dispatch(e)
//    }
//  }

//  def getContract(runMethod: InstanceMethod[Pre], methodName: String): AccountedPredicate[Pre] = {
//    methodName match {
//      case "start" => runMethod.contract.requires
//      case "join" => runMethod.contract.ensures
//      case _ => throw Unreachable("Can only be start or join method")
//    }
//  }

//  def dispatchMethodInvocation(mi: MethodInvocation[Pre], methodName: String): (Expr[Pre], Option[Expr[Pre]], Seq[Variable[Pre]]) = {
//    val runMethod: InstanceMethod[Pre] = getRunMethod(mi)
//    val predicate: Expr[Pre] = unfoldPredicate(getContract(runMethod, methodName)).head
//    val offset: Option[Expr[Pre]] = Some(mi.obj)
//    (predicate, offset)
//  }

  def getDispatchedOffset(e: Eval[Post]) : Expr[Post] = {
    e.expr match {
      case mi: MethodInvocation[Post] => mi.obj
      case _ => ???
    }
  }


  def dispatchJoinInvocation(e: Eval[Pre], mi: MethodInvocation[Pre])(implicit o: Origin = e.o): Statement[Rewritten[Pre]] = {
    val runMethod: InstanceMethod[Pre] = getRunMethod(mi)
    val predicate: Expr[Pre] = unfoldPredicate(runMethod.contract.ensures).head
    val dispatchedStatement: Eval[Post] = super.dispatch(e).asInstanceOf[Eval[Post]]
    val dispatchedOffset: Expr[Post] = getDispatchedOffset(dispatchedStatement)
    val factor = Some(postJoinTokens.top.find(rpj => rpj.obj == dispatchedOffset).get.arg)
    val findoffset = variables.freeze.computeSucc(mi.obj.asInstanceOf[Local[Pre]].ref.decl).get
    //TODO fix that it is not displaying the this but the offset
    val newAddStatements = TransferPermissionRewriter(this, currentClass.top, None, factor, Seq(mi.obj.asInstanceOf[Local[Pre]].ref.decl)).addPermissions(predicate)
    val removeStatements = TransferPermissionRewriter(this, currentClass.top, Some(mi.obj), factor, Seq(mi.obj.asInstanceOf[Local[Pre]].ref.decl)).removePermissions(predicate)
    Block[Post](dispatchedStatement +: (newAddStatements ++ removeStatements))
//    Block[Post](dispatchedStatement +: (removeStatements))
  }

  def dispatchStartInvocation(e: Eval[Pre],mi: MethodInvocation[Pre]): Statement[Rewritten[Pre]] = ???

  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = {
    stat match {
      case rpj: RuntimePostJoin[Pre] if postJoinTokens.nonEmpty => {
        val newRpj = super.dispatch(rpj)
        postJoinTokens.top.addOne(newRpj.asInstanceOf[RuntimePostJoin[Post]])
        newRpj
      }
      case e@Eval(mi: MethodInvocation[Pre]) if isThreadMethod(mi, "join") => dispatchJoinInvocation(e, mi)
//      case e@Eval(mi: MethodInvocation[Pre]) if isThreadMethod(mi, "start") => dispatchStartInvocation(e, mi)
      case _ => super.dispatch(stat)
    }
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
}