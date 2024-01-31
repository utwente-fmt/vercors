package vct.rewrite.runtime.util

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.permissionTransfer.PermissionData


case class FindPermissionLocation[Pre <: Generation](pd: PermissionData[Pre])(implicit program: Program[Pre]) extends Rewriter[Pre] {
  override val allScopes = pd.outer.allScopes

  def getPermission(p: Perm[Pre])(implicit origin: Origin): PermissionLocation[Pre] = {
    p.loc match {
      case a: AmbiguousLocation[Pre] => a.expr match {
        case d: Deref[Pre] => getPermission(d)
        case subscript: AmbiguousSubscript[Pre] => getPermission(subscript)
        case _ => ???
      }
      case _ => ???
    }
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case t: ThisObject[Pre] => pd.getOffset(t)
      case _ => pd.outer.dispatch(e)
    }
  }

  def getPermission(d: Deref[Pre])(implicit origin: Origin): PermissionLocation[Pre] = {
    val permLocation: Expr[Post] = dispatch(d.obj)
    val instanceFieldId: Int = FieldNumber(d.ref.decl)
    NormalLocation[Pre](permLocation, instanceFieldId, pd.threadId)
  }

  def getPermission(subscript: AmbiguousSubscript[Pre])(implicit origin: Origin): PermissionLocation[Pre] = {
    subscript.collection match {
      case d: Deref[Pre] => getPermission(d, subscript)
      case l: Local[Pre] => throw Unreachable("Currently paramaters permissions cannot be checked")
      case _ => throw Unreachable("Currently no other collections are allowed execpt deref")
    }
  }

  def getPermission(d: Deref[Pre], subscript: AmbiguousSubscript[Pre])(implicit origin: Origin): PermissionLocation[Pre] = {
    val permLocation = dispatch(d.obj)
    val instanceFieldId = FieldNumber(d.ref.decl)
    val location = pd.outer.dispatch(subscript.index)
    ArrayLocation[Pre](permLocation, instanceFieldId, location, pd.threadId)
  }
}


sealed trait PermissionLocation[Pre <: Generation] {
  val isArrayPermission: Boolean

  def put(value: Expr[Rewritten[Pre]]): Expr[Rewritten[Pre]]

  def get(): Expr[Rewritten[Pre]]
}

final case class NormalLocation[Pre <: Generation](permLocation: Expr[Rewritten[Pre]], instanceFieldId: Int, threadId: ThreadId[Rewritten[Pre]])(implicit val origin: Origin) extends PermissionLocation[Pre] {
  override val isArrayPermission: Boolean = false

  override def put(value: Expr[Rewritten[Pre]]): Expr[Rewritten[Pre]] = PutPermission(permLocation, instanceFieldId, value, threadId)

  override def get(): Expr[Rewritten[Pre]] = GetPermission[Rewritten[Pre]](permLocation, instanceFieldId, threadId)
}

final case class ArrayLocation[Pre <: Generation](permLocation: Expr[Rewritten[Pre]], instanceFieldId: Int, location: Expr[Rewritten[Pre]], threadId: ThreadId[Rewritten[Pre]])(implicit val origin: Origin) extends PermissionLocation[Pre] {
  override val isArrayPermission: Boolean = true

  override def put(value: Expr[Rewritten[Pre]]): Expr[Rewritten[Pre]] = PutArrayPermission[Rewritten[Pre]](permLocation, instanceFieldId, location, value, threadId)

  override def get(): Expr[Rewritten[Pre]] = GetArrayPermission[Rewritten[Pre]](permLocation, instanceFieldId, location, threadId)
}