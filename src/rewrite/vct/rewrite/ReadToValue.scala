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
      case read @ ReadPerm() => throw WildcardError(read)
      case default => rewriteDefault(default)
    }

}
