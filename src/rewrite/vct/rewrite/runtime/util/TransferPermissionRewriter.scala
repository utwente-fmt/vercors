package vct.rewrite.runtime.util

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.AstBuildHelpers._


case class TransferPermissionRewriter[Pre <: Generation](outer: Rewriter[Pre])() extends Rewriter[Pre] {
  override val allScopes = outer.allScopes


  private def removePermissions(pred: Expr[Pre]): Unit = {
    //TODO: should remove the permissions for the currentThread
    val unfoldedPredicate: Seq[Expr[Pre]] = unfoldStar(pred)
    val newExpressions = unfoldedPredicate.map(dispatch)

    println(unfoldedPredicate)
  }

  private def addPermissions(pred: Expr[Pre]): Unit = {
    //TODO: should add the permissions for the currentThread
    //TODO: maybe move it in a seperate class to add permissions
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case p: Perm[Post] => super.dispatch(p)
      case _ => super.dispatch(e)//TODO fix
    }
  }
}