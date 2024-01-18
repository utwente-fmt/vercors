package vct.rewrite.runtime.util

import vct.col.ast.RewriteHelpers.{RewriteExists, RewriteForall}
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.rewrite.runtime.util.PermissionRewriter._

case class TransferPermissionRewriter[Pre <: Generation](override val outer: Rewriter[Pre], override val cls: Class[Pre], offset: Option[Expr[Pre]], factor: Option[Expr[Rewritten[Pre]]])(implicit program: Program[Pre], newVariables: NewVariableGenerator[Pre], firstRequiredLocals: Seq[Variable[Pre]] = Seq.empty) extends AbstractQuantifierRewriter[Pre](outer, cls, firstRequiredLocals) {

  implicit var add: Boolean = false;

  private def op(a: Expr[Post], b: Expr[Post])(implicit origin: Origin): Expr[Post] = if (add) transA(a) + b else transA(a) - b
  protected def transA(a: Expr[Post])(implicit origin: Origin): Expr[Post] = if (factor.nonEmpty) permissionToRuntimeValue(factor.get) * a else a

  private def unfoldPredicate(predicate: Expr[Pre]): Seq[Expr[Pre]] =
    unfoldStar(predicate).collect { case p@(_: Perm[Pre] | _: Starall[Pre]) => p }

  def addPermissions(predicate: Expr[Pre]) : Seq[Statement[Post]] = {
    add = true;
    transferPermissions(predicate)
  }

  def removePermissions(predicate: Expr[Pre]): Seq[Statement[Post]] = {
    add = false;
    transferPermissions(predicate)
  }

  private def transferPermissions(predicate: Expr[Pre]): Seq[Statement[Post]] = {
    implicit val origin: Origin = predicate.o
    unfoldPredicate(predicate).map(dispatch).map(e => Eval[Post](e))
  }

  private def dispatchPerm(p: Perm[Pre])(implicit origin: Origin): Expr[Post] = {
    val permissionLocation: PermissionLocation[Pre] = FindPermissionLocation[Pre](this, offset)(program, newVariables).getPermission(p)(origin)
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