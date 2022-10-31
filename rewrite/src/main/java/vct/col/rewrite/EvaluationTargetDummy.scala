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

  /**
   * Returns whether it is not possible for the expression to yield verification failures. This is only for output
   * prettification: it does not need to be complete.
   */
  def infallible(e: Expr[Pre]): Boolean = e match {
    case Local(_) => true
    case ADTFunctionInvocation(_, _, args) => args.forall(infallible)

    case Exists(_, _, body) => infallible(body)
    case Forall(_, _, body) => infallible(body)
    case Let(_, value, body) => infallible(value) && infallible(body)

    case Plus(left, right) => infallible(left) && infallible(right)
    case Minus(left, right) => infallible(left) && infallible(right)
    case Mult(left, right) => infallible(left) && infallible(right)
    case And(left, right) => infallible(left) && infallible(right)
    case Or(left, right) => infallible(left) && infallible(right)
    case Select(cond, whenTrue, whenFalse) => infallible(cond) && infallible(whenTrue) && infallible(whenFalse)
    case Not(e) => infallible(e)

    case EitherLeft(e) => infallible(e)
    case EitherRight(e) => infallible(e)
    case LiteralBag(_, es) => es.forall(infallible)
    case LiteralSeq(_, es) => es.forall(infallible)
    case LiteralSet(_, es) => es.forall(infallible)
    case LiteralMap(_, _, pairs) => pairs.forall { case (k, v) => infallible(k) && infallible(v) }
    case LiteralTuple(_, es) => es.forall(infallible)
    case OptSome(e) => infallible(e)
    case Range(from, to) => infallible(from) && infallible(to)

    case BooleanValue(_) | FloatValue(_, _) | IntegerValue(_) => true
    case NoPerm() | Null() | OptNone() | ReadPerm() | Void() | WritePerm() => true

    case _ => false
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case Eval(e) if infallible(e) =>
      Block(Nil)(stat.o)
    case Eval(other) =>
      val v = new Variable[Post](dispatch(other.t))(EvaluationOrigin)
      Scope(Seq(v), assignLocal(v.get(EvaluationOrigin), dispatch(other))(stat.o))(stat.o)
    case other => rewriteDefault(other)
  }
}
