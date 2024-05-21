package vct.col.rewrite

import vct.col.ast._
import vct.col.origin.{DiagnosticOrigin, LabelContext, Origin, PanicBlame, PreferredName}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers.{VarBuildHelpers, function}

case object ResolveExpressionSideChecks extends RewriterBuilder {
  override def key: String = "sideChecks"
  override def desc: String = "Encode with/then annotations that are only the evaluation of an otherwise pure expression."

  private def EvalCheckFunction(preferredName: String = "unknown"): Origin = Origin(
    Seq(
      PreferredName(Seq(preferredName)),
      LabelContext("side check"),
    )
  )
}

case class ResolveExpressionSideChecks[Pre <: Generation]() extends Rewriter[Pre] {
  import ResolveExpressionSideChecks._

  lazy val withEval: Function[Post] = {
    implicit val o: Origin = EvalCheckFunction()

    val t = new Variable[Post](TType(TAnyValue()))(EvalCheckFunction("T"))
    val checkValue = new Variable[Post](TAnyValue())(EvalCheckFunction("checkedValue"))
    val value = new Variable[Post](TVar(t.ref))(EvalCheckFunction("value"))

    globalDeclarations.declare(function[Post](
      blame = PanicBlame("witheval ensures nothing"),
      contractBlame = PanicBlame("witheval requires nothing"),
      typeArgs = Seq(t),
      args = Seq(checkValue, value),
      returnType = TVar(t.ref),
      body = Some(value.get),
    )(EvalCheckFunction("withEval")))
  }

  lazy val thenEval: Function[Post] = {
    implicit val o: Origin = EvalCheckFunction()

    val t = new Variable[Post](TType(TAnyValue()))(EvalCheckFunction("T"))
    val value = new Variable[Post](TVar(t.ref))(EvalCheckFunction("value"))
    val checkValue = new Variable[Post](TAnyValue())(EvalCheckFunction("checkedValue"))

    globalDeclarations.declare(function[Post](
      blame = PanicBlame("theneval ensures nothing"),
      contractBlame = PanicBlame("theneval requires nothing"),
      typeArgs = Seq(t),
      args = Seq(value, checkValue),
      returnType = TVar(t.ref),
      body = Some(value.get),
    )(EvalCheckFunction("thenEval")))
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = e match {
    case With(Eval(check), value) =>
      FunctionInvocation[Post](withEval.ref, Seq(dispatch(check), dispatch(value)), Seq(dispatch(value.t)), Nil, Nil)(
        PanicBlame("witheval requires nothing"))(e.o)
    case Then(value, Eval(check)) =>
      FunctionInvocation[Post](thenEval.ref, Seq(dispatch(value), dispatch(check)), Seq(dispatch(value.t)), Nil, Nil)(
        PanicBlame("theneval requires nothing"))(e.o)
    case other => rewriteDefault(other)
  }
}
