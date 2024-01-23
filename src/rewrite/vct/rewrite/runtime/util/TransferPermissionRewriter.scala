package vct.rewrite.runtime.util

import vct.col.ast.RewriteHelpers.{RewriteExists, RewriteForall}
import vct.col.ast.{Expr, _}
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.rewrite.runtime.util.PermissionRewriter._

case class TransferPermissionRewriter[Pre <: Generation](override val outer: Rewriter[Pre], override val cls: Class[Pre], offset: (Option[Expr[Pre]], Option[Expr[Rewritten[Pre]]]), threadIdExpr: (Option[Expr[Pre]], Option[Expr[Rewritten[Pre]]]), factor: Option[Expr[Rewritten[Pre]]], override val extraArgs: Seq[Variable[Pre]])(implicit program: Program[Pre]) extends AbstractQuantifierRewriter[Pre](outer, cls, extraArgs) {

  implicit var add: Boolean = _

  def addPermissions(predicate: Expr[Pre]): Seq[Statement[Post]] = {
    add = true
    transferPermissions(predicate)
  }

  def removePermissions(predicate: Expr[Pre]): Seq[Statement[Post]] = {
    add = false
    transferPermissions(predicate)
  }

  override def dispatchLoopBody(quantifier: Expr[Pre], left: Expr[Pre], right: Expr[Pre], args: Seq[Variable[Pre]]): Seq[Statement[Post]] = {
    if (add) {
      TransferPermissionRewriter(this, cls, offset, threadIdExpr, factor, args).addPermissions(right)
    } else {
      TransferPermissionRewriter(this, cls, offset, threadIdExpr, factor, args).removePermissions(right)
    }
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

  protected def transA(a: Expr[Post])(implicit origin: Origin): Expr[Post] = if (factor.nonEmpty) permissionToRuntimeValue(factor.get) * a else a

  private def op(a: Expr[Post], b: Expr[Post])(implicit origin: Origin): Expr[Post] = if (add) transA(a) + b else transA(a) - b

  private def unfoldPredicate(predicate: Expr[Pre]): Seq[Expr[Pre]] =
    unfoldStar(predicate).collect { case p@(_: Perm[Pre] | _: Starall[Pre]) => p }

  private def transferPermissions(predicate: Expr[Pre]): Seq[Statement[Post]] = {
    implicit val origin: Origin = predicate.o
    unfoldPredicate(predicate).map(dispatch).map(e => Eval[Post](e))
  }

  private def dispatchPerm(p: Perm[Pre])(implicit origin: Origin): Expr[Post] = {
    val permissionLocation: PermissionLocation[Pre] = FindPermissionLocation[Pre](this, offset, threadIdExpr)(program).getPermission(p)(origin)
    val newValue = permissionToRuntimeValueRewrite(p)
    val getPermission = permissionLocation.get()
    permissionLocation.put(op(getPermission, newValue))
  }
}