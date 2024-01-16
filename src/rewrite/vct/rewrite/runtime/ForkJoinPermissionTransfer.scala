package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.{RewriteInstanceMethod, RewriteScope}
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.rewrite.runtime.util.Util._
import vct.rewrite.runtime.util.{NewVariableGenerator, TransferPermissionRewriter}

import scala.collection.mutable


object ForkJoinPermissionTransfer extends RewriterBuilder {
  override def key: String = "forkJoinPermissionTransfer"

  override def desc: String = "Detects fork/join/run methods and creates a permission transfer for the forked thread"
}


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
      i.body match {
        case Some(sc: Scope[Pre]) => sc.body match {
          case b: Block[Pre] => {
            val newScope = RewriteScope(sc)(this).rewrite(body = dispatchBlock(b,transferPermissionsStatements))
            classDeclarations.succeed(i, i.rewrite(body = Some(newScope)))
          }
        }
      }
    }
  }

  private def dispatchBlock(b: Block[Pre], transferPermissionsStatements: Seq[Statement[Post]] = Seq.empty)(implicit o: Origin): Block[Post] = {
    val (statements, _) = statementBuffer.collect{
      println("adding")
      statementBuffer.top.addAll(transferPermissionsStatements)
      super.dispatch(b)
    }
    println("removing")
    println(statements)
    Block[Post](statements)
  }


  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case mi: MethodInvocation[Pre] if statementBuffer.nonEmpty && isThreadMethod(mi, "join") => dispatchMethodJoin(mi)
      case mi: MethodInvocation[Pre] if statementBuffer.nonEmpty && isThreadMethod(mi, "start") => dispatchMethodStart(mi)
      case _ => super.dispatch(e)
    }
  }

  def dispatchMethodInvocation(mi: MethodInvocation[Pre]): (Expr[Pre], Option[Expr[Post]]) = {
    val runMethod: InstanceMethod[Pre] = getRunMethod(mi)
    val predicate: Expr[Pre] = unfoldPredicate(runMethod.contract.ensures).head
    val offset: Option[Expr[Rewritten[Pre]]] = Some(dispatch(mi.obj))
    (predicate, offset)
  }

  def dispatchMethodJoin(mi: MethodInvocation[Pre]): Expr[Post] = {
    implicit val o: Origin = mi.o
    val (predicate, offset) = dispatchMethodInvocation(mi)
    val factor: Option[Expr[Post]] = Some(postJoinTokens.top.find(rpj => rpj.obj == offset.get).get.arg)
    doPermissionTransfer(predicate, threadId = offset, add = false, offset = offset, factor = factor)
    doPermissionTransfer(predicate, add = true, offset = offset, factor = factor)
    //TODO check that the joining thread has enough permissions to remove the permissions
    super.dispatch(mi)
  }

  def dispatchMethodStart(mi: MethodInvocation[Pre]): Expr[Post] = {
    implicit val o: Origin = mi.o
    val (predicate, offset) = dispatchMethodInvocation(mi)
    //TODO check that the thread has enough permissions to do this operation
    doPermissionTransfer(predicate, offset = offset)
    super.dispatch(mi)
  }

  def doPermissionTransfer(predicate: Expr[Pre], add: Boolean = false, threadId: Option[Expr[Post]] = None, offset: Option[Expr[Post]] = None, factor: Option[Expr[Post]] = None)(implicit o: Origin): Unit = {
    val newStatements: Seq[Statement[Post]] = TransferPermissionRewriter(this, null, ThreadId[Post](threadId), add = add, offset = offset, factor = factor)(program, newVariables.freeze()).transferPermissions(predicate)
    statementBuffer.top.addAll(newStatements)
  }


  private def dispatchRunMethod(i: InstanceMethod[Pre]): Unit = {
    implicit val o: Origin = i.o
    val predicate = unfoldPredicate(i.contract.requires).head
    val transferPermissionsStatements: Seq[Statement[Post]] = TransferPermissionRewriter(this, null, ThreadId[Post](None), add = true)(program, newVariables.freeze()).transferPermissions(predicate)
    dispatchInstanceMethod(i, transferPermissionsStatements)
  }


  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = {
    stat match {
      case rpj: RuntimePostJoin[Pre] if postJoinTokens.nonEmpty => {
        val newRpj = statementBuffer.collect{
          println("new level rpj")
          super.dispatch(rpj)}._2
        postJoinTokens.top.addOne(newRpj.asInstanceOf[RuntimePostJoin[Post]])
        statementBuffer.top.addOne(newRpj)
        newRpj
      }
      case b: Block[Pre] if statementBuffer.nonEmpty => {
        //TODO: FIX
        val newStatement = dispatchBlock(b)(b.o)
        statementBuffer.top.addOne(newStatement)
        newStatement
      }
      case s: Statement[Pre] if statementBuffer.nonEmpty => {
          val newStatement = statementBuffer.collect{
            println("new level normal statement")
            super.dispatch(stat)}._2
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
      case i: InstanceMethod[Pre] if isExtendingThread(currentClass.top) && isMethod(i, "run") => dispatchRunMethod(i)
      case i: InstanceMethod[Pre] => dispatchInstanceMethod(i)
      case _ => super.dispatch(decl)
    }
  }
}