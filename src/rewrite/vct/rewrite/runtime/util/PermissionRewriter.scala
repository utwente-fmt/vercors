package vct.rewrite.runtime.util

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.rewrite.runtime.util.permissionTransfer.PermissionData


object PermissionRewriter {

  def permissionToRuntimeValue[G](perm: Perm[G])(implicit origin: Origin): Expr[G] = {
    permissionToRuntimeValue(perm.perm)
  }

  def permissionToRuntimeValue[G](expr: Expr[G])(implicit origin: Origin): Expr[G] = {
    expr match {
      case _: WritePerm[G] => RuntimeFractionOne()
      case _: ReadPerm[G] => RuntimeFractionZero()
      case d: Div[G] => RuntimeFractionDiff[G](d.left, d.right)
      case d: FloorDiv[G] => RuntimeFractionDiff[G](d.left, d.right)
      case IntegerValue(n: BigInt) if n == 1 => RuntimeFractionOne()
      case _ => expr
    }
  }

  def createCheckPermission[Pre <: Generation](retrievedPermission: Expr[Rewritten[Pre]], permission: Perm[Pre])(implicit origin: Origin): Expr[Rewritten[Pre]] = {
    val newValue = permissionToRuntimeValueRewrite(permission)
    permission.perm match {
      case _: ReadPerm[Pre] => retrievedPermission > newValue
      case _ => retrievedPermission === newValue
    }
  }

  def permissionToRuntimeValueRewrite[Pre <: Generation](permission: Perm[Pre])(implicit origin: Origin): Expr[Rewritten[Pre]] = {
    type Post = Rewritten[Pre]
    val rw = new Rewriter[Pre]()
    permission.perm match {
      case _: WritePerm[Pre] => RuntimeFractionOne()
      case _: ReadPerm[Pre] => RuntimeFractionZero()
      case d: Div[Pre] => RuntimeFractionDiff[Post](rw.dispatch(d.left), rw.dispatch(d.right))
      case d: FloorDiv[Pre] => RuntimeFractionDiff[Post](rw.dispatch(d.left), rw.dispatch(d.right))
      case IntegerValue(n: BigInt) if n == 1 => RuntimeFractionOne()
      case _ => rw.dispatch(permission.perm)
    }
  }

}

case class PermissionRewriter[Pre <: Generation](outer: Rewriter[Pre])(implicit program: Program[Pre]) extends Rewriter[Pre] {
  override val allScopes = outer.allScopes

  def rewritePermission(p: Perm[Pre]): Expr[Rewritten[Pre]] = {
    implicit val origin: Origin = p.o
    val pd: PermissionData[Pre] = PermissionData().setOuter(this)
    val permissionLocation: Expr[Post] = FindPermissionLocation[Pre](pd).getPermission(p)(origin).get()
    PermissionRewriter.createCheckPermission(permissionLocation, p)
  }
}