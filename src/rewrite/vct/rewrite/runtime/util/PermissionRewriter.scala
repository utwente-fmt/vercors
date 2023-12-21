package vct.rewrite.runtime.util

import vct.col.ast._
import vct.col.rewrite.{Generation, NonLatchingRewriter, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.SuccessionMap

object PermissionRewriter{

  def apply[Pre <: Generation](p: Perm[Pre])(implicit program: Program[Pre], newLocals: SuccessionMap[Declaration[_], Variable[Rewritten[Pre]]]): CodeStringCheckPermissionExpr[Rewritten[Pre]] = {
    type Post = Rewritten[Pre]
    val rewriter = new PermissionRewriter[Pre]()

    p.loc match {
      case a: AmbiguousLocation[Pre] => a.expr match {
        case d: Deref[Pre] => {
          val permLocation = rewriter.dispatch(d.obj)
          val instanceFieldId = FieldNumber(d.ref.decl)
          CodeStringCheckPermissionExpr[Post](permLocation, instanceFieldId, rewriter.dispatch(d), rewriter.dispatch(p.perm))(d.o)
        }
        case _ => ???
      }
      case _ => ???
    }
  }
}


case class PermissionRewriter[Pre <: Generation]()(implicit newLocals: SuccessionMap[Declaration[_], Variable[Rewritten[Pre]]]) extends Rewriter[Pre] {
  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      //TODO fix possible change in location (when in a predicate)
      case t : ThisObject[Pre] => super.dispatch(e)
      case _ => super.dispatch(e)
    }
  }
}