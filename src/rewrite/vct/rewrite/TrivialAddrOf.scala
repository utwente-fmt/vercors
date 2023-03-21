package vct.col.rewrite
import vct.col.ast._
import vct.col.origin._
import vct.col.rewrite.TrivialAddrOf.{SubscriptErrorAddError, UnsupportedLocation}
import vct.result.VerificationError.UserError

case object TrivialAddrOf extends RewriterBuilder {
  override def key: String = "trivialAddrOf"
  override def desc: String = "Rewrite trivial instances of the address-of operator & to an expression without it."

  case class SubscriptErrorAddError(sub: PointerSubscript[_]) extends Blame[PointerAddError] {
    override def blame(error: PointerAddError): Unit = error match {
      case err: PointerNull => sub.blame.blame(err)
      case err: PointerBounds => sub.blame.blame(err)
    }
  }

  case class UnsupportedLocation(loc: Expr[_]) extends UserError {
    override def code: String = "wrongAddrOf"
    override def text: String = "Non-trivial instances of the address-of operator are not supported."
  }
}

case class TrivialAddrOf[Pre <: Generation]() extends Rewriter[Pre] {
  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case AddrOf(DerefPointer(p)) =>
      dispatch(p)

    case AddrOf(sub @ PointerSubscript(p, i)) =>
      PointerAdd(dispatch(p), dispatch(i))(SubscriptErrorAddError(sub))(e.o)

    case AddrOf(other) =>
      throw UnsupportedLocation(other)

    case other => rewriteDefault(other)
  }
}
