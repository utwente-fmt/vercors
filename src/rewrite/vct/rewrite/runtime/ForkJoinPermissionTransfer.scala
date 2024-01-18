package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.{RewriteInstanceMethod, RewriteScope}
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.Util._
import vct.rewrite.runtime.util.{NewVariableGenerator, TransferPermissionRewriter}

import scala.collection.mutable


object ForkJoinPermissionTransfer extends RewriterBuilder {
  override def key: String = "forkJoinPermissionTransfer"

  override def desc: String = "Detects fork/join/run methods and creates a permission transfer for the forked thread"
}

case class ForkJoinPermissionTransfer[Pre <: Generation]() extends Rewriter[Pre] {

  implicit var program: Program[Pre] = null

  implicit val newVariables: NewVariableGenerator[Pre] = new NewVariableGenerator[Pre](this)
  val currentClass: ScopedStack[Class[Pre]] = new ScopedStack()
  val statementBuffer: ScopedStack[mutable.ArrayBuffer[Statement[Post]]] = new ScopedStack()
  val postJoinTokens: ScopedStack[mutable.ArrayBuffer[RuntimePostJoin[Post]]] = new ScopedStack()

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    val test = super.dispatch(program)
    test
  }

  private def dispatchRunMethod(i: InstanceMethod[Pre]): Unit = {
    implicit val o: Origin = i.o
    val predicate = unfoldPredicate(i.contract.requires).head
    val transferPermissionsStatements: Seq[Statement[Post]] = TransferPermissionRewriter(this, currentClass.top, None, None).addPermissions(predicate)
    dispatchInstanceMethod(i, transferPermissionsStatements)
  }

  def dispatchMethodStart(mi: MethodInvocation[Pre]): Expr[Post] = {
    implicit val o: Origin = mi.o
    val (predicate, offset, variable) = dispatchMethodInvocation(mi, "start")
//    implicit val firstRequiredLocals: Seq[Variable[Pre]] = variable
    //TODO check that the thread has enough permissions to do this operation
    //    doPermissionTransfer(predicate, offset = offset, firstLocals = variable)
    val newStatements = TransferPermissionRewriter(this, currentClass.top, None, None).removePermissions(predicate)
    statementBuffer.top.addAll(newStatements)
    super.dispatch(mi)
  }

  def dispatchMethodJoin(mi: MethodInvocation[Pre]): Expr[Post] = {
    implicit val o: Origin = mi.o
    val (predicate, offset, variable) = dispatchMethodInvocation(mi, "join")
    val dispatchedOffset = dispatch(offset.get)
    val pjts = postJoinTokens.top.toSeq
    val factor: Option[Expr[Post]] = Some(postJoinTokens.top.find(rpj => rpj.obj == dispatchedOffset).get.arg)
    implicit val firstRequiredLocals: Seq[Variable[Pre]] = variable
    val newAddStatements = TransferPermissionRewriter(this, currentClass.top, None, factor).addPermissions(predicate)
    val removeStatements = TransferPermissionRewriter(this, currentClass.top, offset, factor).removePermissions(predicate)
    statementBuffer.top.addAll(removeStatements)
    statementBuffer.top.addAll(newAddStatements)
    //    doPermissionTransfer(predicate, offset = offset, factor = factor)
    //    doPermissionTransfer(predicate, offset = offset, factor = factor, firstLocals = variable)
    //TODO check that the joining thread has enough permissions to remove the permissions
    super.dispatch(mi)
  }


  protected def dispatchInstanceMethod(i: InstanceMethod[Pre], transferPermissionsStatements: Seq[Statement[Post]] = Seq.empty): Unit = {
    implicit val o: Origin = i.o

      postJoinTokens.collect {
        val newArgs = i.args.map(newVariables.createNew)
        val newContract = dispatch(i.contract)
        newVariables.collect {
        i.body match {
          case Some(sc: Scope[Pre]) => sc.body match {
            case b: Block[Pre] => {
              val newLocals = sc.locals.map(newVariables.createNew)
              val newScope = RewriteScope(sc)(this).rewrite(locals = newLocals, body = dispatchBlock(b, transferPermissionsStatements))
              classDeclarations.succeed(i, i.rewrite(args= newArgs, contract= newContract, body = Some(newScope)))
            }
          }
        }
      }.result
    }
  }

  private def dispatchBlock(b: Block[Pre], transferPermissionsStatements: Seq[Statement[Post]] = Seq.empty)(implicit o: Origin): Block[Post] = {
    val (statements, _) = statementBuffer.collect{
      statementBuffer.top.addAll(transferPermissionsStatements)
      super.dispatch(b)
    }
    Block[Post](statements)
  }


  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case mi: MethodInvocation[Pre] if statementBuffer.nonEmpty && isThreadMethod(mi, "join") => dispatchMethodJoin(mi)
      case mi: MethodInvocation[Pre] if statementBuffer.nonEmpty && isThreadMethod(mi, "start") => dispatchMethodStart(mi)
      case l: Local[Pre] if newVariables.nonEmpty => newVariables.getLocal(l)
      case _ => super.dispatch(e)
    }
  }

  def getContract(runMethod: InstanceMethod[Pre], methodName: String): AccountedPredicate[Pre] = {
    methodName match {
      case "start" => runMethod.contract.requires
      case "join" => runMethod.contract.ensures
      case _ => throw Unreachable("Can only be start or join method")
    }
  }

  def dispatchMethodInvocation(mi: MethodInvocation[Pre], methodName: String): (Expr[Pre], Option[Expr[Pre]], Seq[Variable[Pre]]) = {
    val runMethod: InstanceMethod[Pre] = getRunMethod(mi)
    val predicate: Expr[Pre] = unfoldPredicate(getContract(runMethod, methodName)).head
    val offset: Option[Expr[Pre]] = Some(mi.obj)
    val variable = collectPossibleVariable(mi)
    (predicate, offset, variable)
  }

  def collectPossibleVariable(mi: MethodInvocation[Pre]) : Seq[Variable[Pre]] = {
    mi.obj match {
      case l: Local[Pre] => Seq(l.ref.decl)
      case _ => Seq.empty
    }
  }



//  def doPermissionTransfer(predicate: Expr[Pre], offset: Option[Expr[Pre]] = None, factor: Option[Expr[Post]] = None, firstLocals: Seq[Variable[Pre]] = Seq.empty)(implicit o: Origin): Unit = {
//    val newStatements: Seq[Statement[Post]] = TransferPermissionRewriter(this, currentClass.top, offset = offset, factor = factor)(program, newVariables, firstLocals).transferPermissions(predicate)
//    statementBuffer.top.addAll(newStatements)
//  }





  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = {
    stat match {
      case rpj: RuntimePostJoin[Pre] if postJoinTokens.nonEmpty => {
        val newRpj = super.dispatch(rpj)
        postJoinTokens.top.addOne(newRpj.asInstanceOf[RuntimePostJoin[Post]])
        statementBuffer.top.addOne(newRpj)
        newRpj
      }
      case b: Block[Pre] if statementBuffer.nonEmpty => {
        val newStatement = dispatchBlock(b)(b.o)
        statementBuffer.top.addOne(newStatement)
        newStatement
      }
      case s: Statement[Pre] if statementBuffer.nonEmpty => {
          val newStatement = super.dispatch(stat)
          statementBuffer.top.addOne(newStatement)
          newStatement
      }
      case _ => super.dispatch(stat)
    }
  }



  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case cls: Class[Pre] => currentClass.having(cls) {
        super.dispatch(cls)
      }
      case v: Variable[Pre] if newVariables.nonEmpty => {
        newVariables.createNew(v)
      }
      case i: InstanceMethod[Pre] if isExtendingThread(currentClass.top) && isMethod(i, "run") => dispatchRunMethod(i)
      case i: InstanceMethod[Pre] => dispatchInstanceMethod(i)
      case _ => super.dispatch(decl)
    }
  }
}