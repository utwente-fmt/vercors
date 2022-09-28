package vct.col.rewrite

import vct.col.ast._
import RewriteHelpers._
import vct.col.origin.Origin
import vct.col.util.AstBuildHelpers._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

case object EvaluationTargetDummy extends RewriterBuilder {
  override def key: String = "evaluationTarget"
  override def desc: String = "Make a target to assign to for evaluations that contain no side effects, but must be well-formed regardless."

  case object EvaluationOrigin extends Origin {
    override def preferredName: String = "evaluationDummy"
    override def shortPosition: String = "generated"
    override def context: String = s"[At variable generated for an evaluation]"
    override def inlineContext: String = "[Variable generated for an evaluation]"
  }
}

case class EvaluationTargetDummy[Pre <: Generation]() extends Rewriter[Pre] {
  import EvaluationTargetDummy._

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case Eval(_: ProcedureInvocation[Pre] | _: MethodInvocation[Pre]) => rewriteDefault(stat)
    case Eval(other) =>
      val v = new Variable[Post](dispatch(other.t))(EvaluationOrigin)
      Scope(Seq(v), assignLocal(v.get(EvaluationOrigin), dispatch(other))(stat.o))(stat.o)
    case other => rewriteDefault(other)
  }
}
