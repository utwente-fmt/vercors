package vct.col.rewrite

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}

import scala.annotation.tailrec

case object SplitQuantifiers extends RewriterBuilder {
  override def key: String = "splitQuantifiers"
  override def desc: String = "Split quantifiers with multiple bindings into separate nested quantifiers to make the simplifier more composable."
}

case class SplitQuantifiers[Pre <: Generation]() extends Rewriter[Pre] {
  @tailrec
  private def unroll(bindings: Seq[Variable[Pre]], body: Expr[Post], builder: (Variable[Post], Expr[Post]) => Expr[Post]): Expr[Post] =
    bindings match {
      case Nil => body
      case binding :: bindings => unroll(bindings, builder(variables.dispatch(binding), body), builder)
    }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = e match {
    case Forall(bindings, triggers, body) =>
      variables.scope {
        unroll(
          bindings.init,
          Forall(Seq(variables.dispatch(bindings.last)), triggers.map(_.map(dispatch)), dispatch(body))(e.o),
          (binding, body) => Forall(Seq(binding), Nil, body)(e.o)
        )
      }
    case s @ Starall(bindings, triggers, body) =>
      variables.scope {
        unroll(
          bindings.init,
          Starall(Seq(variables.dispatch(bindings.last)), triggers.map(_.map(dispatch)), dispatch(body))(s.blame)(e.o),
          (binding, body) => Starall(Seq(binding), Nil, body)(s.blame)(e.o)
        )
      }
    case Exists(bindings, triggers, body) =>
      variables.scope {
        unroll(
          bindings.init,
          Exists(Seq(variables.dispatch(bindings.last)), triggers.map(_.map(dispatch)), dispatch(body))(e.o),
          (binding, body) => Exists(Seq(binding), Nil, body)(e.o)
        )
      }
    case other => rewriteDefault(other)
  }
}
