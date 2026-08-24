package vct.col.rewrite
import vct.col.ast._
import vct.col.origin._
import vct.col.util.AstBuildHelpers.{ExprBuildHelpers, const}
import vct.result.VerificationError.UserError

case object TrivialAddrOf extends RewriterBuilder {
  override def key: String = "trivialAddrOf"
  override def desc: String =
    "Rewrite trivial instances of the address-of operator & to an expression without it."

  case class UnsupportedLocation(loc: Expr[_]) extends UserError {
    override def code: String = "wrongAddrOf"
    override def text: String =
      loc.o.messageInContext(
        "Non-trivial instances of the address-of operator are not supported."
      )
  }
}

case class TrivialAddrOf[Pre <: Generation]() extends Rewriter[Pre] {
  import TrivialAddrOf._

  override def dispatch(e: Expr[Pre]): Expr[Post] =
    e match {
      case DerefPointer(PointerAdd(AddrOf(pointer), offset))
          if offset.isInstanceOf[ConstantInt[Pre]] &&
            offset.asInstanceOf[ConstantInt[Pre]].value.signum == 0 =>
        dispatch(pointer)
      case AddrOf(DerefPointer(p)) => dispatch(p)
      case DerefPointer(AddrOf(p)) => dispatch(p)
      case AddrOf(sub @ PointerSubscript(p, i)) =>
        PointerAdd(dispatch(p), dispatch(i))(PointerSubscriptToAddBlame(
          sub.blame
        ))(e.o)
      // Handled by EncodePointerArrays
      case AddrOf(PointerArraySubscript(_, _)) => e.rewriteDefault()
      case AddrOf(Deref(_, _)) => e.rewriteDefault()
      // Nullable PointerArrays (i.e. those in parameters) are not special cased in EncodePointerArrays
      case AddrOf(other)
          if other.t.asPointerArray.isEmpty ||
            !other.t.asPointerArray.get.isNonNull =>
        throw UnsupportedLocation(other)
      case assign @ PreAssignExpression(target, AddrOf(value))
          if value.t.asByReferenceClass.isDefined =>
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
      case other => other.rewriteDefault()
    }

  override def dispatch(s: Statement[Pre]): Statement[Post] =
    s match {
      case assign @ Assign(target, AddrOf(value))
          if value.t.asByReferenceClass.isDefined =>
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
      case other => other.rewriteDefault()
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
    val newPointer =
      Assign(
        newTarget,
        NewNonNullPointer(newValue.t, const[Post](1), None)(PanicBlame(
          "Size is > 0"
        )),
      )(blame)
    (newPointer, newTarget, newValue)
  }
}
