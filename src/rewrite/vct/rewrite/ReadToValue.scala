package vct.col.rewrite

import vct.col.ast._
import vct.col.rewrite.ReadToValue.WildcardError
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError

object ReadToValue extends RewriterBuilder {

  case class WildcardError(node: ReadPerm[_]) extends UserError {
    override def code: String = "wildcard"

    override def text: String =
      node.o.messageInContext(
        "You cannot have a wildcard outside of a permission statement."
      )
  }

  override def key: String = "readToValue"

  override def desc: String =
    "Translates \"read\" into wildcard for permission."
}

case class ReadToValue[Pre <: Generation]() extends Rewriter[Pre] {

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case Perm(loc, ReadPerm()) => Value(dispatch(loc))(expr.o)
      case cp @ ChorPerm(endpoint, loc, ReadPerm()) =>
        implicit val o = cp.o
        EndpointExpr(succ(endpoint.decl), Value(dispatch(loc)))
      case Scale(ReadPerm(), Perm(loc, WritePerm())) =>
        // Temporary solution for predicates: there should be a proper notion of scaling by read instead.
        Value(dispatch(loc))(expr.o)
      case read @ ReadPerm() => throw WildcardError(read)
      case default => default.rewriteDefault()
    }

  override def dispatch(target: FoldTarget[Pre]): FoldTarget[Post] =
    target match {
      case ScaledPredicateApply(inv, ReadPerm()) =>
        ValuePredicateApply(dispatch(inv))(target.o)
      case other => other.rewriteDefault()
    }
}
