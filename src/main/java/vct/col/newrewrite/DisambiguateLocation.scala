package vct.col.newrewrite

import vct.col.ast.{AmbiguousLocation, ArrayLocation, ArraySubscript, Deref, Expr, FieldLocation, InstancePredicateApply, InstancePredicateLocation, Location, ModelDeref, ModelLocation, PointerLocation, PredicateApply, PredicateLocation, SilverDeref, SilverFieldLocation, WritePerm}
import vct.col.newrewrite.DisambiguateLocation.NotALocation
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError

case object DisambiguateLocation extends RewriterBuilder {

  case class NotALocation(expr: Expr[_]) extends UserError {
    override def code: String = "notALocation"

    override def text: String = expr.o.messageInContext("This expression is not a heap location.")
  }
  override def key: String = "disambiguateLocation"

  override def desc: String = "Translate ambiguous location type into concrete location type."
}

case class DisambiguateLocation[Pre <: Generation]() extends Rewriter[Pre]  {
  override def dispatch(loc: Location[Pre]): Location[Post] = {
    implicit val o: Origin = loc.o
    loc match {
      case location@AmbiguousLocation(expr) => {
        expr match {
          case Deref(obj, ref) =>
            FieldLocation(dispatch(obj), succ(ref.decl))
          case ModelDeref(obj, ref) =>
            ModelLocation(dispatch(obj), succ(ref.decl))
          case SilverDeref(obj, ref) =>
            SilverFieldLocation(dispatch(obj), succ(ref.decl))
          case expr@ArraySubscript(arr, index) =>
            ArrayLocation(dispatch(arr), dispatch(index))(expr.blame)
          case point@expr if expr.t.asPointer.isDefined =>
            PointerLocation(dispatch(expr))(location.blame)
          case PredicateApply(ref, args, WritePerm()) =>
            PredicateLocation(succ(ref.decl), (args.map(dispatch)))
          case InstancePredicateApply(obj, ref, args, WritePerm()) =>
            InstancePredicateLocation(succ(ref.decl), dispatch(obj), args.map(dispatch))
          case default =>
            throw NotALocation(default)
        }
      }
      case other => rewriteDefault(other)
    }
  }
}
