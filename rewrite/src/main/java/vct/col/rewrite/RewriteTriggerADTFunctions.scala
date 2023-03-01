package vct.col.rewrite

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers.VarBuildHelpers

case object RewriteTriggerADTFunctions extends RewriterBuilder {
  override def key: String = "rewriteTriggerADTFunctions"
  override def desc: String = "Rewrite adt forwarding functions in triggers to axiomatic ones."
}

case class RewriteTriggerADTFunctions[Pre <: Generation]() extends Rewriter[Pre]  {

    val inTrigger: ScopedStack[Unit] = ScopedStack()

    override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
      case f @ Forall(_, triggers, _) =>
        f.rewrite(triggers = inTrigger.having(()) {
          triggers.map(_.map(dispatch))
        })

      case s @ Starall(_, triggers, _) =>
        s.rewrite(triggers = inTrigger.having(()) {
          triggers.map(_.map(dispatch))
        })

      case e @ Exists(_, triggers, _) =>
        e.rewrite(triggers = inTrigger.having(()) {
          triggers.map(_.map(dispatch))
        })

      case other => rewriteDefault(other)

    }
}
