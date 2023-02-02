package vct.col.rewrite

import vct.col.ast._
import vct.col.rewrite.DisambiguateLocation.NotALocation
import vct.col.origin.{Blame, Origin, PointerLocationError}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError

case object DisambiguateLocation extends RewriterBuilder {

  case class NotALocation(expr: Expr[_]) extends UserError {
    override def code: String = "notALocation"

    def hint: Option[String] = expr match {
      case PointerSubscript(_, _) => Some(" (Hint: perhaps you meant to prepend `&`)")
      case DerefPointer(_) => Some(" (Hint: perhaps you meant to prepend `&`)")
      case _ => None
    }

    override def text: String = expr.o.messageInContext("This expression is not a heap location." + hint.getOrElse(""))
  }
  override def key: String = "disambiguateLocation"

  override def desc: String = "Translate ambiguous location type into concrete location type."
}

case class DisambiguateLocation[Pre <: Generation]() extends Rewriter[Pre]  {
  def exprToLoc(expr: Expr[Pre], blame: Blame[PointerLocationError])(implicit o: Origin): Location[Post] = expr match {
    case Deref(obj, ref) =>
      FieldLocation(dispatch(obj), succ(ref.decl))
    case ModelDeref(obj, ref) =>
      ModelLocation(dispatch(obj), succ(ref.decl))
    case SilverDeref(obj, ref) =>
      SilverFieldLocation(dispatch(obj), succ(ref.decl))
    case expr@ArraySubscript(arr, index) =>
      ArrayLocation(dispatch(arr), dispatch(index))(expr.blame)
    case expr if expr.t.asPointer.isDefined =>
      PointerLocation(dispatch(expr))(blame)
    case PredicateApply(ref, args, WritePerm()) =>
      PredicateLocation(succ(ref.decl), (args.map(dispatch)))
    case InstancePredicateApply(obj, ref, args, WritePerm()) =>
      InstancePredicateLocation(succ(ref.decl), dispatch(obj), args.map(dispatch))

    case InlinePattern(inner, _, _) => exprToLoc(inner, blame)

    case default =>
      throw NotALocation(default)
  }

  override def dispatch(loc: Location[Pre]): Location[Post] = loc match {
    case location@AmbiguousLocation(expr) => exprToLoc(expr, location.blame)(loc.o)
    case other => rewriteDefault(other)
  }
}
