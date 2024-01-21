package vct.rewrite.runtime.util

import vct.col.ast.{Expr, _}
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable


case class FindPermissionLocation[Pre <: Generation](outer: Rewriter[Pre], offset: Option[Expr[Pre]])(implicit program: Program[Pre]) extends Rewriter[Pre] {
  override val allScopes = outer.allScopes

  val threadId : ThreadId[Post] = {
    if(offset.isEmpty) {
      ThreadId[Post](None)(Origin(Seq.empty))
    }else {
      val expr = offset.get
      val inner = expr match {
        case l: Local[Pre] => Some(variables.freeze.computeSucc(l.ref.decl).get.get(expr.o))
        case _ => None
      }
      ThreadId[Post](inner)(Origin(Seq.empty))
    }
  }

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
    val permLocation: Expr[Post] = outer.dispatch(d.obj)
    val instanceFieldId: Int = FieldNumber(d.ref.decl)
    NormalLocation[Pre](permLocation, instanceFieldId, threadId)
  }

  private def getPermission(subscript: AmbiguousSubscript[Pre])(implicit origin: Origin): PermissionLocation[Pre] = {
    subscript.collection match {
      case d: Deref[Pre] => getPermission(d, subscript)
      case l: Local[Pre] =>throw Unreachable("Currently paramaters permissions cannot be checked")
      case _ => throw Unreachable("Currently no other collections are allowed execpt deref")
    }
  }

  private def getPermission(d: Deref[Pre], subscript: AmbiguousSubscript[Pre])(implicit origin: Origin): PermissionLocation[Pre] = {
    val permLocation = outer.dispatch(d.obj)
    val instanceFieldId = FieldNumber(d.ref.decl)
    val location = outer.dispatch(subscript.index)
    ArrayLocation[Pre](permLocation, instanceFieldId, location, threadId)
  }

//  private def getPermission(l: Local[Pre], subscript: AmbiguousSubscript[Pre])(implicit origin: Origin): PermissionLocation[Pre] = {
//    val permLocation = this.dispatch(l)
//    val instanceFieldId = FieldNumber(d.ref.decl)
//    val location = this.dispatch(subscript.index)
//    ArrayLocation[Pre](permLocation, instanceFieldId, location, threadId)
//    ArrayLocation[Pre]()
//  }


  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      //TODO fix possible change in location (when in a predicate)
      case t: ThisObject[Pre] => {
        offset.map(dispatch).getOrElse(outer.dispatch(e))
      }
      case _ => outer.dispatch(e)
    }
  }
}


sealed trait PermissionLocation[Pre <: Generation] {
  def put(value: Expr[Rewritten[Pre]]): Expr[Rewritten[Pre]]

  def get(): Expr[Rewritten[Pre]]

  val isArrayPermission: Boolean
}

final case class NormalLocation[Pre <: Generation](permLocation: Expr[Rewritten[Pre]], instanceFieldId: Int, threadId: ThreadId[Rewritten[Pre]])(implicit val origin: Origin) extends PermissionLocation[Pre] {
  override def put(value: Expr[Rewritten[Pre]]): Expr[Rewritten[Pre]] = PutPermission(permLocation, instanceFieldId, value, threadId)

  override def get(): Expr[Rewritten[Pre]] = GetPermission[Rewritten[Pre]](permLocation, instanceFieldId, threadId)

  override val isArrayPermission: Boolean = false
}

final case class ArrayLocation[Pre <: Generation](permLocation: Expr[Rewritten[Pre]], instanceFieldId: Int, location: Expr[Rewritten[Pre]], threadId: ThreadId[Rewritten[Pre]])(implicit val origin: Origin) extends PermissionLocation[Pre] {
  override def put(value: Expr[Rewritten[Pre]]): Expr[Rewritten[Pre]] = PutArrayPermission[Rewritten[Pre]](permLocation, instanceFieldId, location, value, threadId)

  override def get(): Expr[Rewritten[Pre]] = GetArrayPermission[Rewritten[Pre]](permLocation, instanceFieldId, location, threadId)

  override val isArrayPermission: Boolean = true
}