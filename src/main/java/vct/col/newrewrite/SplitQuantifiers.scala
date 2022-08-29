package vct.col.newrewrite

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}

import scala.annotation.tailrec

case object SplitQuantifiers extends RewriterBuilder {
  override def key: String = "splitQuantifiers"
  override def desc: String = "Split quantifiers with multiple bindings into separate nested quantifiers to make the simplifier more composable."
}

case class SplitQuantifiers[Pre <: Generation]() extends Rewriter[Pre] {
  @tailrec
  private def unroll(bindings: Seq[Variable[Pre]], body: Expr[Post], builder: (Variable[Post], Boolean, Expr[Post]) => Expr[Post]): Expr[Post] =
    bindings match {
      case binding :: Nil => builder(variables.dispatch(binding), true, body)
      case binding :: more => unroll(more, builder(variables.dispatch(binding), false, body), builder)
    }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = e match {
    case Forall(bindings, triggers, body) =>
      variables.scope {
        unroll(bindings, dispatch(body), (binding, outer, body) => Forall(Seq(binding), if(outer) triggers.map(_.map(dispatch)) else Nil, body)(e.o))
      }
    case s @ Starall(bindings, triggers, body) =>
      variables.scope {
        unroll(bindings, dispatch(body), (binding, outer, body) => Starall(Seq(binding), if (outer) triggers.map(_.map(dispatch)) else Nil, body)(s.blame)(e.o))
      }
    case Exists(bindings, triggers, body) =>
      variables.scope {
        unroll(bindings, dispatch(body), (binding, outer, body) => Exists(Seq(binding), if (outer) triggers.map(_.map(dispatch)) else Nil, body)(e.o))
      }
    case other => rewriteDefault(other)
  }
}
