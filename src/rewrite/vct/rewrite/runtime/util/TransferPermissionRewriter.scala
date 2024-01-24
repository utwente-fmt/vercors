package vct.rewrite.runtime.util

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.Generation
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.AbstractQuantifierRewriter.LoopBodyContent
import vct.rewrite.runtime.util.PermissionRewriter._
import vct.rewrite.runtime.util.permissionTransfer.PermissionData

case class TransferPermissionRewriter[Pre <: Generation](pd: PermissionData[Pre])(implicit program: Program[Pre]) extends AbstractQuantifierRewriter[Pre](pd) {

  implicit var add: Boolean = _

  def addPermissions(predicate: Expr[Pre]): Block[Post] = {
    add = true
    transferPermissions(predicate)
  }

  def removePermissions(predicate: Expr[Pre]): Block[Post] = {
    add = false
    transferPermissions(predicate)
  }

  override def dispatchLoopBody(loopBodyContent: LoopBodyContent[Pre])(implicit origin: Origin): Block[Post] = {
    if (add) {
      TransferPermissionRewriter(pd).addPermissions(loopBodyContent.expr)
    } else {
      TransferPermissionRewriter(pd).removePermissions(loopBodyContent.expr)
    }
  }


  def dispatchExpr(e: Expr[Pre]): Statement[Post] = {
    implicit val origin: Origin = e.o
    e match {
      case p: Perm[Pre] => Eval[Post](dispatchPerm(p))
      case s: Starall[Pre] => super.dispatchQuantifier(s) //Let the AbstractQuantifier rewrite the StarAll, since it is the only one that can hold permissions
      case _ => Block[Post](Seq.empty)
    }
  }

  private def op(a: Expr[Post], b: Expr[Post])(implicit origin: Origin): Expr[Post] = if (add) pd.factored(a) + b else pd.factored(a) - b

  private def unfoldPredicate(predicate: Expr[Pre]): Seq[Expr[Pre]] =
    unfoldStar(predicate).collect { case p@(_: Perm[Pre] | _: Starall[Pre]) => p }

  private def transferPermissions(predicate: Expr[Pre]): Block[Post] = {
    implicit val origin: Origin = predicate.o
    Block[Post](unfoldPredicate(predicate).map(dispatchExpr))
  }

  private def dispatchPerm(p: Perm[Pre])(implicit origin: Origin): Expr[Post] = {
    val permissionLocation: PermissionLocation[Pre] = FindPermissionLocation[Pre](pd)(program).getPermission(p)(origin)
    val newValue = permissionToRuntimeValueRewrite(p)
    val getPermission = permissionLocation.get()
    permissionLocation.put(op(getPermission, newValue))
  }
}