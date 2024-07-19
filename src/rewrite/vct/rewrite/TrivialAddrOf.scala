package vct.col.rewrite
import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.TrivialAddrOf.{
  SubscriptErrorAddError,
  UnsupportedLocation,
}
import vct.col.util.AstBuildHelpers.{ExprBuildHelpers, const}
import vct.result.VerificationError.UserError

case object TrivialAddrOf extends RewriterBuilder {
  override def key: String = "trivialAddrOf"
  override def desc: String =
    "Rewrite trivial instances of the address-of operator & to an expression without it."

  case class SubscriptErrorAddError(sub: PointerSubscript[_])
      extends Blame[PointerAddError] {
    override def blame(error: PointerAddError): Unit =
      error match {
        case err: PointerNull => sub.blame.blame(err)
        case err: PointerBounds => sub.blame.blame(err)
      }
  }

  case class UnsupportedLocation(loc: Expr[_]) extends UserError {
    override def code: String = "wrongAddrOf"
    override def text: String =
      "Non-trivial instances of the address-of operator are not supported."
  }
}

case class TrivialAddrOf[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(e: Expr[Pre]): Expr[Post] =
    e match {
      case AddrOf(DerefPointer(p)) => dispatch(p)

      case AddrOf(sub @ PointerSubscript(p, i)) =>
        PointerAdd(dispatch(p), dispatch(i))(SubscriptErrorAddError(sub))(e.o)

      case AddrOf(other) => throw UnsupportedLocation(other)
      case assign @ PreAssignExpression(target, AddrOf(value))
          if value.t.isInstanceOf[TClass[Pre]] =>
        implicit val o: Origin = assign.o
        val (newPointer, newTarget, newValue) = rewriteAssign(
          target,
          value,
          assign.blame,
          assign.o,
        )
        val newAssign =
          PreAssignExpression(
            PointerSubscript(newTarget, const[Post](0))(PanicBlame(
              "Should always be accessible"
            )),
            newValue,
          )(assign.blame)
        With(newPointer, newAssign)
      case other => rewriteDefault(other)
    }

  override def dispatch(s: Statement[Pre]): Statement[Post] =
    s match {
      case assign @ Assign(target, AddrOf(value))
          if value.t.isInstanceOf[TClass[Pre]] =>
        implicit val o: Origin = assign.o
        val (newPointer, newTarget, newValue) = rewriteAssign(
          target,
          value,
          assign.blame,
          assign.o,
        )
        val newAssign =
          Assign(
            PointerSubscript(newTarget, const[Post](0))(PanicBlame(
              "Should always be accessible"
            )),
            newValue,
          )(assign.blame)
        Block(Seq(newPointer, newAssign))
      case other => rewriteDefault(other)
    }

  // TODO: AddressOff needs a more structured approach. Now you could assign a local structure to a pointer, and that pointer
  //  keeps the information, whilst in normal C this would be garbage collected away after exiting the function. E.g.
  //  void test(struct d* y){
  //    struct d x;
  //    y = &x;
  //  }
  def rewriteAssign(
      target: Expr[Pre],
      value: Expr[Pre],
      blame: Blame[AssignFailed],
      assignO: Origin,
  ): (Statement[Post], Expr[Post], Expr[Post]) = {
    implicit val o: Origin = assignO
    val newTarget = dispatch(target)
    val newValue = dispatch(value)
    val newPointer = Eval(
      PreAssignExpression(
        newTarget,
        NewPointerArray(newValue.t, const[Post](1), None)(PanicBlame("Size is > 0")),
      )(blame)
    )
    (newPointer, newTarget, newValue)
  }
}
