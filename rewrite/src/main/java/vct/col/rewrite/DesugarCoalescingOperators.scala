package vct.col.rewrite

import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

object DesugarCoalescingOperators extends RewriterBuilder{

  override def key: String = "coalesce"

  override def desc: String = "Translating the ?. operator to an implication"
}

case class DesugarCoalescingOperators[Pre <:Generation]() extends Rewriter[Pre] {

  override def dispatch(e: Expr[Pre]): Expr[Post] = {
    implicit val o : Origin = e.o
    e match {
      case CoalesceInstancePredicateApply(obj, ref, args, perm) =>
        Implies(
          Neq(dispatch(obj), Null()),
          InstancePredicateApply(dispatch(obj), succ(ref.decl), args.map(dispatch), dispatch(perm)))
      case other => rewriteDefault(other)
    }
  }

}
