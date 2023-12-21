package vct.rewrite.runtime.util

import vct.col.ast._
import vct.col.rewrite.{Generation, NonLatchingRewriter, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.SuccessionMap


case class PermissionRewriter[Pre <: Generation](outer: Rewriter[Pre])(implicit program: Program[Pre], newLocals: NewVariableResult[Pre, _]) extends Rewriter[Pre] {
  override val allScopes = outer.allScopes

  def rewritePermission(p: Perm[Pre]): Expr[Rewritten[Pre]] = {
    p.loc match {
      case a: AmbiguousLocation[Pre] => a.expr match {
        case d: Deref[Pre] => {
          val permLocation = this.dispatch(d.obj)
          val instanceFieldId = FieldNumber(d.ref.decl)
          CodeStringCheckPermissionExpr[Post](permLocation, instanceFieldId, this.dispatch(d), this.dispatch(p.perm))(d.o)
        }
        case subscript: AmbiguousSubscript[Pre] => {
          subscript.collection match {
            case d: Deref[Pre] => {
              val permLocation = this.dispatch(d.obj)
              val instanceFieldId = FieldNumber(d.ref.decl)

              val location = this.dispatch(subscript.index)
              CodeStringCheckArrayPermissionExpr[Post](permLocation, instanceFieldId, location, this.dispatch(d), this.dispatch(p.perm))(d.o)
            }
            case _ => ???
          }

        }
        case _ => ???
      }
      case _ => ???
    }
  }


  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      //TODO fix possible change in location (when in a predicate)
      case t: ThisObject[Pre] => super.dispatch(e)
      case l : Local[Pre] => {
        Local[Post](newLocals.mapping.ref(l.ref.decl))(l.o)
      }
      case _ => super.dispatch(e)
    }
  }
}