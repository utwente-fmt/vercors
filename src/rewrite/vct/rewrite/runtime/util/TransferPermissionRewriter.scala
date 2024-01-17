package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers.{RewriteExists, RewriteForall}
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.PermissionRewriter._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class TransferPermissionRewriter[Pre <: Generation](override val outer: Rewriter[Pre], override val cls: Class[Pre], threadId: ThreadId[Rewritten[Pre]], add: Boolean, offset: Option[Expr[Rewritten[Pre]]] = None, factor: Option[Expr[Rewritten[Pre]]] = None)(implicit program: Program[Pre], newVariables: NewVariableGenerator[Pre], firstRequiredLocals: Seq[Variable[Pre]] = Seq.empty) extends AbstractQuantifierRewriter[Pre](outer, cls, firstRequiredLocals) {

  private def transA(a: Expr[Post])(implicit origin: Origin): Expr[Post] = if (factor.nonEmpty) permissionToRuntimeValue(factor.get) * a else a

  private def op(a: Expr[Post], b: Expr[Post])(implicit origin: Origin): Expr[Post] = if (add) transA(a) + b else transA(a) - b

  private def unfoldPredicate(pred: Expr[Pre]): Seq[Expr[Pre]] =
    unfoldStar(pred).collect { case p@(_: Perm[Pre] | _: Starall[Pre]) => p }


  def transferPermissions(pred: Expr[Pre]): Seq[Statement[Post]] = {
    implicit val origin: Origin = pred.o
    unfoldPredicate(pred).map(dispatch).map(e => Eval[Post](e))
  }

  private def dispatchPerm(p: Perm[Pre])(implicit origin: Origin): Expr[Post] = {
    val permissionLocation: PermissionLocation[Pre] = FindPermissionLocation[Pre](this, threadId, offset)(program, newVariables).getPermission(p)(origin)
    val newValue = permissionToRuntimeValueRewrite(p)
    val getPermission = permissionLocation.get()
    permissionLocation.put(op(getPermission, newValue))
  }


  override def dispatchLoopBody(quantifier: Expr[Pre], left: Expr[Pre], right: Expr[Pre]): Seq[Statement[Post]] = {
    transferPermissions(right)
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    implicit val origin: Origin = e.o
    e match {
      case p: Perm[Pre] => dispatchPerm(p)
      case f: Forall[Pre] => f.rewrite() // Normal rewrite, so it will not create a new quantifier code
      case ex: Exists[Pre] => ex.rewrite() // Normal rewrite, so it will not create a new quantifier code
      case s: Starall[Pre] => super.dispatch(s) //Let the AbstractQuantifier rewrite the StarAll, since it is the only one that can hold permissions
      case _ => super.dispatch(e)
    }
  }
}