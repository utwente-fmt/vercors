package vct.rewrite

import vct.col.ast._
import vct.col.origin.{
  Blame,
  Origin,
  PointerAddError,
  PointerBounds,
  PointerLocationError,
  PointerNull,
  PointerSubscriptError,
  PointerSubscriptToAddBlame,
  TypeName,
}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError

case object DisambiguateLocation extends RewriterBuilder {

  private case class NotALocation(expr: Expr[_]) extends UserError {
    override def code: String = "notALocation"

    private def hint: Option[String] =
      if (expr.t.asPointer.isDefined) {
        expr match {
          case AddrOf(_) =>
            Some(
              " (Hint: perhaps you meant to access this location, i.e. remove the `&`)"
            )
          case _ =>
            Some(" (Hint: perhaps you meant to dereference this pointer)")
        }
      } else { None }

    override def text: String =
      expr.o.messageInContext(
        "This expression is not a heap location." + hint.getOrElse("")
      )
  }

  private case class InvalidPatternLocation(expr: Expr[_], cls: Class[_])
      extends UserError {
    override def code: String = "byValueClassLocationPattern"

    override def text: String =
      expr.o.messageInContext(
        s"A ${cls.o.find[TypeName].map(_.name).getOrElse("class")} value (which will be expanded to permissions for every field) is not allowed in a trigger pattern"
      )
  }

  private case class IncompletePermissionDereference(expr: Expr[_])
      extends UserError {
    override def code: String = "incompleteArrayDereference"

    override def text: String =
      expr.o.messageInContext(
        "Getting permission to a partially dereferenced array is not supported, permission can only be gotten for actual elements"
      )
  }

  override def key: String = "disambiguateLocation"

  override def desc: String =
    "Translate ambiguous location type into concrete location type."
}

case class DisambiguateLocation[Pre <: Generation]() extends Rewriter[Pre] {
  import DisambiguateLocation._

  private def exprToLoc(expr: Expr[Pre])(implicit o: Origin): Location[Post] =
    expr match {
      case InlinePattern(inner, pattern, group) =>
        if (inner.t.asByValueClass.isDefined) {
          throw InvalidPatternLocation(
            expr,
            inner.t.asByValueClass.get.cls.decl,
          )
        }
        InLinePatternLocation(
          exprToLoc(inner),
          InlinePattern(dispatch(inner), pattern, group)(expr.o),
        )(expr.o)
      case expr if expr.t.asByValueClass.isDefined =>
        ByValueClassLocation(dispatch(expr))
      case dp @ DerefPointer(p) =>
        if (p.t.asPointerArray.exists(_.dimensions.length != 1)) {
          throw IncompletePermissionDereference(dp)
        } else { PointerLocation(dispatch(p))(dp.blame) }
      case pas @ PointerArraySubscript(arr, _) =>
        if (arr.t.asPointerArray.get.dimensions.length == 1) {
          PointerLocation(PointerAdd(dispatch(pas.array), dispatch(pas.index))(
            PointerSubscriptToAddBlame(pas.blame)
          ))(pas.blame)
        } else { throw IncompletePermissionDereference(pas) }
      case ps @ PointerSubscript(p, index) =>
        PointerLocation(PointerAdd(dispatch(p), dispatch(index))(
          PointerSubscriptToAddBlame(ps.blame)
        ))(ps.blame)
      case DerefHeapVariable(ref) => HeapVariableLocation(succ(ref.decl))
      case Deref(obj, ref) => FieldLocation(dispatch(obj), succ(ref.decl))
      case ModelDeref(obj, ref) => ModelLocation(dispatch(obj), succ(ref.decl))
      case SilverDeref(obj, ref) =>
        SilverFieldLocation(dispatch(obj), succ(ref.decl))
      case expr @ ArraySubscript(arr, index) =>
        ArrayLocation(dispatch(arr), dispatch(index))(expr.blame)
      case PredicateApplyExpr(inv) => PredicateLocation(dispatch(inv))
      case default => throw NotALocation(default)
    }

  override def dispatch(loc: Location[Pre]): Location[Post] =
    loc match {
      case location @ AmbiguousLocation(expr) => exprToLoc(expr)(loc.o)
      case other => super.dispatch(other)
    }
}
