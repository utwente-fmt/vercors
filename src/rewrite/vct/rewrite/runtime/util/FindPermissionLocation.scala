package vct.rewrite.runtime.util

import vct.col.ast.{Expr, _}
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable


case class FindPermissionLocation[Pre <: Generation](outer: Rewriter[Pre], offset: Option[Expr[Rewritten[Pre]]] = None )(implicit program: Program[Pre], newLocals: NewVariableResult[Pre, _]) extends Rewriter[Pre] {
  override val allScopes = outer.allScopes

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

  private def getPermission(d: Deref[Pre])(implicit origin: Origin): PermissionLocation[Pre] = {
    val permLocation: Expr[Post] = this.dispatch(d.obj)
    val instanceFieldId: Int = FieldNumber(d.ref.decl)
    NormalLocation[Pre](permLocation, instanceFieldId)
  }

  private def getPermission(subscript: AmbiguousSubscript[Pre])(implicit origin: Origin): PermissionLocation[Pre] = {
    subscript.collection match {
      case d: Deref[Pre] => getPermission(d, subscript)
      case _ => throw Unreachable("Currently no other collections are allowed execpt deref")
    }
  }

  private def getPermission(d: Deref[Pre], subscript: AmbiguousSubscript[Pre])(implicit origin: Origin): PermissionLocation[Pre] = {
    val permLocation = this.dispatch(d.obj)
    val instanceFieldId = FieldNumber(d.ref.decl)
    val location = this.dispatch(subscript.index)
    ArrayLocation[Pre](permLocation, instanceFieldId, location)
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      //TODO fix possible change in location (when in a predicate)
      case t: ThisObject[Pre] => offset.getOrElse(super.dispatch(e))
      case l: Local[Pre] => {
        Local[Post](newLocals.mapping.ref(l.ref.decl))(l.o)
      }
      case _ => super.dispatch(e)
    }
  }
}


sealed trait PermissionLocation[Pre <: Generation] {
  def put(value: Expr[Rewritten[Pre]]): Expr[Rewritten[Pre]]
  def get(): Expr[Rewritten[Pre]]
  val isArrayPermission: Boolean
}

final case class NormalLocation[Pre <: Generation](permLocation: Expr[Rewritten[Pre]], instanceFieldId: Int)(implicit val origin: Origin) extends PermissionLocation[Pre] {
  override def put(value: Expr[Rewritten[Pre]]): Expr[Rewritten[Pre]] = PutPermission(permLocation, instanceFieldId, value)
  override def get(): Expr[Rewritten[Pre]] = GetPermission[Rewritten[Pre]](permLocation, instanceFieldId)
  override val isArrayPermission: Boolean = false
}

final case class ArrayLocation[Pre <: Generation](permLocation: Expr[Rewritten[Pre]], instanceFieldId: Int, location: Expr[Rewritten[Pre]])(implicit val origin: Origin) extends PermissionLocation[Pre] {
  override def put(value: Expr[Rewritten[Pre]]): Expr[Rewritten[Pre]] = PutArrayPermission[Rewritten[Pre]](permLocation, instanceFieldId, location, value)
  override def get(): Expr[Rewritten[Pre]] = GetArrayPermission[Rewritten[Pre]](permLocation, instanceFieldId, location)
  override val isArrayPermission: Boolean = true
}