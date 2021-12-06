package vct.col.newrewrite

import vct.col.ast._
import RewriteHelpers._
import vct.col.origin.Origin
import vct.col.util.AstBuildHelpers._
import vct.col.rewrite.Rewriter

case object EvaluationTargetDummy {
  case object EvaluationOrigin extends Origin {
    override def preferredName: String = "evaluationDummy"
    override def messageInContext(message: String): String =
      s"[At variable generated for an evaluation]: $message"
  }
}

case class EvaluationTargetDummy() extends Rewriter {
  import EvaluationTargetDummy._

  override def dispatch(stat: Statement): Statement = stat match {
    case Eval(_: ProcedureInvocation | _: MethodInvocation) => rewriteDefault(stat)
    case Eval(other) =>
      val v = new Variable(other.t)(EvaluationOrigin)
      v.declareDefault(this)
      Assign(v.get(EvaluationOrigin), dispatch(other))(stat.o)
    case other => rewriteDefault(other)
  }
}
