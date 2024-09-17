package vct.col.rewrite

import vct.col.ast._
import vct.col.rewrite.DisambiguateLocation.NotALocation
import vct.col.origin.{
  Blame,
  Origin,
  PanicBlame,
  PointerAddError,
  PointerBounds,
  PointerLocationError,
  PointerNull,
}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers.const
import vct.result.VerificationError.UserError

case object DisambiguateLocation extends RewriterBuilder {

  case class NotALocation(expr: Expr[_]) extends UserError {
    override def code: String = "notALocation"

    def hint: Option[String] =
      expr match {
        case PointerSubscript(_, _) =>
          Some(" (Hint: perhaps you meant to prepend `&`)")
        case DerefPointer(_) =>
          Some(" (Hint: perhaps you meant to prepend `&`)")
        case _ => None
      }

    override def text: String =
      expr.o.messageInContext(
        "This expression is not a heap location." + hint.getOrElse("")
      )
  }

  case class PointerAddRedirect(blame: Blame[PointerLocationError])
      extends Blame[PointerAddError] {
    override def blame(error: PointerAddError): Unit =
      error match {
        case nil: PointerNull => blame.blame(nil)
        // It should not be possible to acquire an out-of-bounds pointer and pass it around
        case bounds: PointerBounds =>
          PanicBlame("Got location of pointer that was out of bounds")
      }
  }

  override def key: String = "disambiguateLocation"

  override def desc: String =
    "Translate ambiguous location type into concrete location type."
}

case class DisambiguateLocation[Pre <: Generation]() extends Rewriter[Pre] {
  import DisambiguateLocation._

  def exprToLoc(expr: Expr[Pre], blame: Blame[PointerLocationError])(
      implicit o: Origin
  ): Location[Post] =
    expr match {
      case expr if expr.t.asPointer.isDefined =>
        expr match {
          case e: PointerAdd[Pre] => PointerLocation(dispatch(e))(blame)
          // Adding ptr + 0 for triggering purposes (is there a better place to do this transformation?)
          case e =>
            PointerLocation(
              PointerAdd[Post](dispatch(e), const[Post](0))(PointerAddRedirect(
                blame
              ))
            )(blame)
        }

      case expr if expr.t.isInstanceOf[TByValueClass[Pre]] =>
        ByValueClassLocation(dispatch(expr))(blame)
      case DerefHeapVariable(ref) => HeapVariableLocation(succ(ref.decl))
      case Deref(obj, ref) => FieldLocation(dispatch(obj), succ(ref.decl))
      case ModelDeref(obj, ref) => ModelLocation(dispatch(obj), succ(ref.decl))
      case SilverDeref(obj, ref) =>
        SilverFieldLocation(dispatch(obj), succ(ref.decl))
      case expr @ ArraySubscript(arr, index) =>
        ArrayLocation(dispatch(arr), dispatch(index))(expr.blame)
      case PredicateApplyExpr(inv) => PredicateLocation(dispatch(inv))
      case InlinePattern(inner, pattern, group) =>
        InLinePatternLocation(
          exprToLoc(inner, blame),
          InlinePattern(dispatch(inner), pattern, group)(expr.o),
        )(expr.o)

      case default => throw NotALocation(default)
    }

  override def dispatch(loc: Location[Pre]): Location[Post] =
    loc match {
      case location @ AmbiguousLocation(expr) =>
        exprToLoc(expr, location.blame)(loc.o)
      case other => rewriteDefault(other)
    }
}
