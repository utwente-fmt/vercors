package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.{RewriteInstanceMethod, RewriteScope}
import vct.col.ast.{AccountedPredicate, Block, Class, CodeStringStatement, Declaration, Deref, Div, Expr, InstanceMethod, IntegerValue, Perm, Program, ReadPerm, Scope, Statement, WritePerm}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.CodeStringDefaults._
import vct.rewrite.runtime.util.{FieldNumber, FieldObjectString}

import scala.collection.mutable

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

//  val classes: ScopedStack[Class[Pre]] = ScopedStack()
//  val permissionExprContract: ScopedStack[Seq[CodeStringStatement[Post]]] = ScopedStack()
//  val permDeref: ScopedStack[Deref[Pre]] = ScopedStack()
//
//  val fieldFinder: ScopedStack[FieldNumber[Pre]] = ScopedStack()
//
//  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
//    fieldFinder.having(FieldNumber[Pre](program)) {
//      val test = super.dispatch(program)
//      test
//    }
//  }
//
//  override def dispatch(decl: Declaration[Pre]): Unit = {
//    decl match {
//      case im: InstanceMethod[Pre] => dispatchInstanceMethod(im)
//      case cls: Class[Pre] => classes.having(cls){super.dispatch(decl)}
//      case _ => super.dispatch(decl)
//    }
//  }
//
//  def dispatchInstanceMethod(im: InstanceMethod[Pre]): Unit = {
//    im.o.getPreferredNameOrElse() match {
//      case "run" => dispatchInstanceMethodRun(im)
//      case _ => super.dispatch(im);
//    }
//  }
//
//  def dispatchInstanceMethodRun(im: InstanceMethod[Pre]): Unit = {
//    if(isRunnable(classes.top)){
//      assignRequiredPermissions(im)
//    }else {
//      super.dispatch(im)
//    }
//  }
//
//  def isRunnable(cls: Class[Pre]) : Boolean = {
//    cls.supports.map(c => c.decl.o.getPreferredNameOrElse()).contains("Runnable")
//  }
//
//  def assignRequiredPermissions(im: InstanceMethod[Pre]): Unit = {
//    im.rewrite(
//      body = rewriteBodyInstanceMethod(im)
//    )
//  }
//
//  def rewriteBodyInstanceMethod(im: InstanceMethod[Pre]): Option[Statement[Post]] = {
//    im.body match {
//      case Some(sc: Scope[Pre]) => sc match {
//        case block: Block[Pre] => Some(sc.rewrite(body = rewriteMethodStatements(block, im)))
//      }
//      case _ => None
//    }
//  }
//
//  def rewriteMethodStatements(block: Block[Pre], im: InstanceMethod[Pre]): Block[Post] = {
//    permissionExprContract.having(Seq[CodeStringStatement[Post]]){
//      dispatch(im.contract.requires);
//      Block[Post](permissionExprContract.top ++ block.statements.map(dispatch))(block.o)
//    }
//  }
//
//  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
//    e match {
//      case p: Perm[Pre] => {
//        if (!permissionExprContract.isEmpty) {
//          permDeref.having(null) {
//            val res = super.dispatch(e)
//            val newTop = permissionExprContract.top :+ createTransferCode(permDeref.top, p)
//            permissionExprContract.pop()
//            permissionExprContract.push(newTop)
//            res
//          }
//        } else {
//          super.dispatch(e)
//        }
//      }
//      case d: Deref[Pre] => {
//        if (!permDeref.isEmpty && permDeref.top == null) {
//          permDeref.pop()
//          permDeref.push(d)
//        }
//        super.rewriteDefault(d)
//
//      }
//      case _ => super.dispatch(e)
//    }
//  }
//
//  private def createTransferCode(deref: Deref[Pre], p: Perm[Pre]): CodeStringStatement[Post] = {
//    val name: String = FieldObjectString().determineObjectReference(deref)
//    val id: Int = fieldFinder.top.findNumber(deref.ref.decl)
//    p.perm match {
//      case iv: IntegerValue[Pre] => {
//        if (iv.value > 1) {
//          throw Unreachable("Permission cannot be exceeding 1")
//        }
//        CodeStringStatement(takePermissionInteger(name, id, p.perm.toString))(p.o)
//      }
//      case d: Div[Pre] => CodeStringStatement(takePermissionDiv(name, id, fractionTemplate(d.left.toString, d.right.toString)))(p.o)
//      case _: WritePerm[Pre] => CodeStringStatement(takePermissionWrite(name, id))(p.o)
//      case _: ReadPerm[Pre] => CodeStringStatement(takePermissionRead(name, id))(p.o)
//      case _ => ???
//    }
//  }

}