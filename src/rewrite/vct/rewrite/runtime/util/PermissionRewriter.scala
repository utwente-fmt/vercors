package vct.rewrite.runtime.util

import vct.col.ast.{Div, Expr, GetPermission, _}
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, NonLatchingRewriter, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.SuccessionMap
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable


object PermissionRewriter {

  def permissionToRuntimeValue[Pre <: Generation](permission: Perm[Pre])(implicit origin: Origin): Expr[Rewritten[Pre]] = {
    type Post = Rewritten[Pre]
    val rw = new Rewriter[Pre]()
    permission.perm match {
      case _: WritePerm[Pre] => const(1)
      case _: ReadPerm[Pre] => const(0)
      case d: Div[Pre] => RuntimeFractionGet[Post](rw.dispatch(d.left), rw.dispatch(d.right))
      case _ => rw.dispatch(permission.perm)
    }
  }

  def createCheckPermission[Pre <: Generation](retrievedPermission: Expr[Rewritten[Pre]], permission: Perm[Pre])(implicit origin: Origin): Expr[Rewritten[Pre]] = {
    val newValue = permissionToRuntimeValue(permission)
    permission.perm match {
      case _: ReadPerm[Pre] => retrievedPermission > newValue
      case _ => retrievedPermission === newValue
    }
  }

}

case class PermissionRewriter[Pre <: Generation](outer: Rewriter[Pre])(implicit program: Program[Pre], newLocals: NewVariableResult[Pre, _]) extends Rewriter[Pre] {
  override val allScopes = outer.allScopes

  def rewritePermission(p: Perm[Pre]): Expr[Rewritten[Pre]] = {
    implicit val origin: Origin = p.o
    val permissionLocation : Expr[Post] = FindPermissionLocation[Pre](this).getPermission(p).get()
    PermissionRewriter.createCheckPermission(permissionLocation, p)
  }

//  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
//    e match {
//      //TODO fix possible change in location (when in a predicate)
//      case t: ThisObject[Pre] => super.dispatch(e)
//      case l: Local[Pre] => {
//        Local[Post](newLocals.mapping.ref(l.ref.decl))(l.o)
//      }
//      case _ => super.dispatch(e)
//    }
//  }
}