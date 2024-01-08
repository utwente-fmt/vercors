package vct.rewrite.runtime.util

import vct.col.ast.{Div, Expr, GetPermission, _}
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, NonLatchingRewriter, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.SuccessionMap
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable


case class PermissionRewriter[Pre <: Generation](outer: Rewriter[Pre])(implicit program: Program[Pre], newLocals: NewVariableResult[Pre, _]) extends Rewriter[Pre] {
  override val allScopes = outer.allScopes

  def rewritePermission(p: Perm[Pre]): Expr[Rewritten[Pre]] = {
    implicit val origin: Origin = p.o
    val permission = p.loc match {
      case a: AmbiguousLocation[Pre] => a.expr match {
        case d: Deref[Pre] => getPermission(d)
        case subscript: AmbiguousSubscript[Pre] => getPermission(subscript)
        case _ => ???
      }
      case _ => ???
    }
    createCheckPermission(permission, p)
  }

  private def createCheckPermission(retrievedPermission: Expr[Post], permission: Perm[Pre])(implicit origin: Origin): Expr[Post] ={
    permission.perm match {
      case _: WritePerm[Pre] => retrievedPermission === const(1)
      case _: ReadPerm[Pre] => retrievedPermission > const(0)
      case d: Div[Pre] => retrievedPermission === RuntimeFractionGet[Post](dispatch(d.left), dispatch(d.right))
      case _ => retrievedPermission === dispatch(permission.perm)
    }
  }

  private def getPermission(d: Deref[Pre])(implicit origin: Origin): Expr[Post] = {
    val permLocation = this.dispatch(d.obj)
    val instanceFieldId = FieldNumber(d.ref.decl)
    GetPermission[Post](permLocation, instanceFieldId)
  }

  private def getPermission(subscript: AmbiguousSubscript[Pre])(implicit origin: Origin): Expr[Post] = {
    subscript.collection match {
      case d: Deref[Pre] => getPermission(d, subscript)
      case _ => throw Unreachable("Currently no other collections are allowed execpt deref")
    }
  }

  private def getPermission(d: Deref[Pre], subscript: AmbiguousSubscript[Pre])(implicit origin: Origin): Expr[Post] = {
    val permLocation = this.dispatch(d.obj)
    val instanceFieldId = FieldNumber(d.ref.decl)
    val location = this.dispatch(subscript.index)
    GetArrayPermission[Post](permLocation, instanceFieldId, location)
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      //TODO fix possible change in location (when in a predicate)
      case t: ThisObject[Pre] => super.dispatch(e)
      case l: Local[Pre] => {
        Local[Post](newLocals.mapping.ref(l.ref.decl))(l.o)
      }
      case _ => super.dispatch(e)
    }
  }
}