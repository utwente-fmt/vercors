package vct.col.newrewrite

import vct.col.ast._
import vct.col.newrewrite.ResolveScale.{CheckScale, ScaleNegativePreconditionFailed, WrongScale}
import vct.col.origin.{Blame, NoContext, Origin, PanicBlame, PreconditionFailed, ScaleNegative}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._
import vct.result.VerificationError.UserError

case object ResolveScale extends RewriterBuilder {
  override def key: String = "scale"
  override def desc: String = "Inline the scale operator into resource expressions."

  case class WrongScale(scale: Expr[_]) extends UserError {
    override def code: String = "wrongScale"
    override def text: String =
      scale.o.messageInContext("This kind of expression cannot be scaled.")
  }

  case class CheckScale(preferredName: String = "") extends Origin {
    override def shortPosition: String = "generated"
    override def context: String = "[At function generated to check that scale values are non-negative]"
    override def inlineContext: String = "[Function generated to check that scale values are non-negative]"
  }

  case class ScaleNegativePreconditionFailed(scale: Scale[_]) extends Blame[PreconditionFailed] {
    override def blame(error: PreconditionFailed): Unit =
      scale.blame.blame(ScaleNegative(scale))
  }
}

case class ResolveScale[Pre <: Generation]() extends Rewriter[Pre] {
  lazy val checkScaleFunc: Function[Post] = {
    implicit val o: Origin = CheckScale()

    val v = new Variable[Post](TRational())(CheckScale("amount"))

    function[Post](
      blame = PanicBlame("scale ensures nothing"),
      contractBlame = PanicBlame("scale only requires a positive rational"),
      args = Seq(v),
      returnType = TRational(),
      body = Some(v.get),
      requires = UnitAccountedPredicate(v.get >= const(0)),
    )(CheckScale("scale")).declareDefault(this)
  }

  def scaleValue(e: Scale[Pre]): Expr[Post] =
    FunctionInvocation(checkScaleFunc.ref[Function[Post]], Seq(dispatch(e.scale)), Nil, Nil, Nil)(
      NoContext(ScaleNegativePreconditionFailed(e)))(e.scale.o)

  def scale(res: Expr[Pre], amount: Expr[Post]): Expr[Post] = {
    implicit val o: Origin = res.o
    res match {
      case s: Scale[Pre] => scale(s.res, amount * scaleValue(s))

      case e if TBool().superTypeOf(e.t) => dispatch(e)
      case Perm(loc, p) => Perm(dispatch(loc), amount * dispatch(p))
      case Value(loc) =>
        (amount > NoPerm()) ==> Value(dispatch(loc))
      case apply: PredicateApply[Pre] => apply.rewrite(perm = amount * dispatch(apply.perm))

      case Star(left, right) => scale(left, amount) &* scale(right, amount)
      case Implies(cond, cons) => Implies(dispatch(cond), scale(cons, amount))
      case Select(cond, whenTrue, whenFalse) => Select(dispatch(cond), scale(whenTrue, amount), scale(whenFalse, amount))
      case s: Starall[Pre] => s.rewrite(body = scale(s.body, amount))

      case other => throw WrongScale(other)
    }
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    implicit val o: Origin = e.o
    e match {
      case s: Scale[Pre] =>
        scale(s.res, scaleValue(s))
      case other => rewriteDefault(other)
    }
  }
}
