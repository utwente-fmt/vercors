package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.{RewriteBlock, RewriteClass, RewriteInstanceMethod, RewriteScope}
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.CodeStringDefaults._
import vct.rewrite.runtime.util.Util._
import vct.rewrite.runtime.util.{FieldNumber, FieldObjectString, NewVariableGenerator, NewVariableResult, TransferPermissionRewriter}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object CreateForkPermissionTransfer extends RewriterBuilder {
  override def key: String = "createForkPermissionTransfrer"

  override def desc: String = "Detects fork methods and creates a permission transfer for the forked thread"
}


case class CreateForkPermissionTransfer[Pre <: Generation]() extends Rewriter[Pre] {

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

  def threadClasses: Seq[Class[Pre]] = collectThreadClasses(program.declarations)


  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    val test = super.dispatch(program)
    test
  }


  private def dispatchRunMethod(i: InstanceMethod[Pre]): Unit = {
    val predicate = unfoldPredicate(i.contract.requires).head
    val transferPermissionsStatements: Seq[Statement[Post]] = TransferPermissionRewriter(this, null, add = true)(program, newVariables.freeze()).transferPermissions(predicate)
    dispatchInstanceMethod(i, transferPermissionsStatements)
  }

  private def dispatchInstanceMethod(i: InstanceMethod[Pre], transferPermissionsStatements: Seq[Statement[Post]] = Seq.empty): Unit = {
    implicit val o: Origin = i.o
    statementBuffer.collect {
      statementBuffer.top.addAll(transferPermissionsStatements)
      i.body match {
        case Some(sc: Scope[Pre]) => sc.body match {
          case b: Block[Pre] => {
            val newScope = RewriteScope(sc)(this).rewrite(body = dispatchBlock(b))
            classDeclarations.succeed(i, i.rewrite(body = Some(newScope)))
          }
        }
      }
    }
  }

  private def dispatchBlock(b: Block[Pre])(implicit o: Origin): Block[Post] = {
    super.dispatch(b)
    Block[Post](statementBuffer.top.toSeq)
  }

  def dispatchMethodInvocation(mi: MethodInvocation[Pre]): Expr[Post] = {
    val runMethod: InstanceMethod[Pre] = getRunMethod(mi)
    val predicate = unfoldPredicate(runMethod.contract.requires).head
    //TODO check that the thread has enough permissions to do this operation
    val removePermissions: Seq[Statement[Post]] = TransferPermissionRewriter(this, null, add = false)(program, newVariables.freeze()).transferPermissions(predicate)
    statementBuffer.top.addAll(removePermissions)
    super.dispatch(mi)
  }


  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case mi: MethodInvocation[Pre] if statementBuffer.nonEmpty && isStartThreadMethod(mi) => dispatchMethodInvocation(mi)
      case _ => super.dispatch(e)
    }
  }


  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = {
    stat match {
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
      case i: InstanceMethod[Pre] if isExtendingThread(currentClass.top) && isRunMethod(i) => dispatchRunMethod(i)
      case i: InstanceMethod[Pre] => dispatchInstanceMethod(i)
      case _ => super.dispatch(decl)
    }
  }
}