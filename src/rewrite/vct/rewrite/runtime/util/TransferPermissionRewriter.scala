package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.Unreachable

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class TransferPermissionRewriter[Pre <: Generation](override val outer: Rewriter[Pre], override val cls: Class[Pre], add: Boolean, offset: Option[Expr[Rewritten[Pre]]] = None)(implicit program: Program[Pre], newLocals: NewVariableResult[Pre, _]) extends AbstractQuantifierRewriter[Pre](outer, cls) {


  private def op(a: Expr[Post], b: Expr[Post])(implicit origin: Origin): Expr[Post] = if (add) a + b else a - b

  private def unfoldPredicate(pred: Expr[Pre]): Seq[Expr[Pre]] =
    unfoldStar(pred).collect { case p@(_: Perm[Pre] | _: Starall[Pre]) => p }


  def transferPermissions(pred: Expr[Pre]): Seq[Statement[Post]] = {
    implicit val origin: Origin = pred.o
    unfoldPredicate(pred).map(dispatch).map(e => Eval[Post](e))
  }

  private def dispatchPerm(p: Perm[Pre])(implicit origin: Origin): Expr[Post] = {
    val permissionLocation: PermissionLocation[Pre] = FindPermissionLocation[Pre](this, offset).getPermission(p)(origin)
    val newValue = PermissionRewriter.permissionToRuntimeValue(p)
    val getPermission = permissionLocation.get()
    permissionLocation.put(op(getPermission, newValue))
  }


  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    implicit val origin: Origin = e.o
    e match {
      case p: Perm[Pre] => dispatchPerm(p)
      case s: Starall[Pre] => super.dispatch(s) //TODO fix permissions transfer for the *all quantifier since these are the only once that can hold a permission
      case _ => super.dispatch(e)
    }
  }
}