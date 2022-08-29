package vct.col.newrewrite

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.result.VerificationError.UserError

object ReadToValue extends RewriterBuilder {

  case class WildcardError() extends UserError {
    override def code: String = "wildcard"

    override def text: String = "Cannot use wildcards outside of permissions."
  }


  override def key: String = "readToValue"

  override def desc: String = "Translates \"read\" into wildcard for permission"
}

class ReadToValue[Pre <: Generation] extends Rewriter[Pre]{

  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case Perm(loc, ReadPerm()) =>
      Value(dispatch(loc))(expr.o)
    case ReadPerm() =>
    case default => rewriteDefault(default)
  }

}
