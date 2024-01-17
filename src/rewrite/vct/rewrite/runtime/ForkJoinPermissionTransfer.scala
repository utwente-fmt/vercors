package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.{RewriteInstanceMethod, RewriteScope}
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.Util._
import vct.rewrite.runtime.util.{NewVariableGenerator, TransferPermissionRewriter}

import scala.collection.mutable


object ForkJoinPermissionTransfer extends RewriterBuilder {
  override def key: String = "forkJoinPermissionTransfer"

  override def desc: String = "Detects fork/join/run methods and creates a permission transfer for the forked thread"
}



//TODO: let the factor and stuff not being created here but in the transferpermission rewriter, because the variables and locals cannot be used in the quantifier if it is dispatched here


case class ForkJoinPermissionTransfer[Pre <: Generation]() extends Rewriter[Pre] {

  /*
    When a fork occurs:
    Class C implements Runnable {
      private int x;


      requires Perm(this.x, 1\2);
      public void run(){
        this method should start with assigning the permissions to itself
        this method needs access to x thus it requires read permission to x
        for(int i = 0; i < x; i++) {
          int a = i * x;
        }
      }
    }

    C c = new C();
    Thread thread = new Thread(C);
    thread.start();

   */
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

  def dispatchMethodInvocation(mi: MethodInvocation[Pre], methodName: String): (Expr[Pre], Option[Expr[Rewritten[Pre]]], Seq[Variable[Pre]]) = {
    val runMethod: InstanceMethod[Pre] = getRunMethod(mi)
    val predicate: Expr[Pre] = unfoldPredicate(getContract(runMethod, methodName)).head
    val offset: Option[Expr[Rewritten[Pre]]] = Some(dispatch(mi.obj))
    val variable = collectPossibleVariable(mi)
    (predicate, offset, variable)
  }

  def collectPossibleVariable(mi: MethodInvocation[Pre]) : Seq[Variable[Pre]] = {
    mi.obj match {
      case l: Local[Pre] => Seq(l.ref.decl)
      case _ => Seq.empty
    }
  }

  def dispatchMethodJoin(mi: MethodInvocation[Pre]): Expr[Post] = {
    implicit val o: Origin = mi.o
    val (predicate, offset, variable) = dispatchMethodInvocation(mi, "join")
    val factor: Option[Expr[Post]] = Some(postJoinTokens.top.find(rpj => rpj.obj == offset.get).get.arg)
    doPermissionTransfer(predicate, threadId = offset, add = false, offset = offset, factor = factor)
    doPermissionTransfer(predicate, add = true, offset = offset, factor = factor, firstLocals = variable)
    //TODO check that the joining thread has enough permissions to remove the permissions
    super.dispatch(mi)
  }

  def dispatchMethodStart(mi: MethodInvocation[Pre]): Expr[Post] = {
    implicit val o: Origin = mi.o
    val (predicate, offset, variable) = dispatchMethodInvocation(mi, "start")
    //TODO check that the thread has enough permissions to do this operation
    doPermissionTransfer(predicate, offset = offset, firstLocals = variable)
    super.dispatch(mi)
  }

  def doPermissionTransfer(predicate: Expr[Pre], add: Boolean = false, threadId: Option[Expr[Post]] = None, offset: Option[Expr[Post]] = None, factor: Option[Expr[Post]] = None, firstLocals: Seq[Variable[Pre]] = Seq.empty)(implicit o: Origin): Unit = {
    val newStatements: Seq[Statement[Post]] = TransferPermissionRewriter(this, currentClass.top, ThreadId[Post](threadId), add = add, offset = offset, factor = factor)(program, newVariables, firstLocals).transferPermissions(predicate)
    statementBuffer.top.addAll(newStatements)
  }


  private def dispatchRunMethod(i: InstanceMethod[Pre]): Unit = {
    implicit val o: Origin = i.o
    val predicate = unfoldPredicate(i.contract.requires).head
    val transferPermissionsStatements: Seq[Statement[Post]] = TransferPermissionRewriter(this, currentClass.top, ThreadId[Post](None), add = true)(program, newVariables).transferPermissions(predicate)
    dispatchInstanceMethod(i, transferPermissionsStatements)
  }


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