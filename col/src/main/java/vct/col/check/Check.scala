package vct.col.check

import vct.col.ast._
import vct.col.err.ASTStateError
import vct.col.ref.Ref

sealed trait CheckError {
  override def toString: String = this match {
    case TypeError(expr, expectedType) =>
      expr.o.messageInContext(s"Expected the type of this expression to be `$expectedType`, but got ${expr.t}.")
    case TypeErrorText(expr, message) =>
      expr.o.messageInContext(message(expr.t))
    case GenericTypeError(t, expectedType) =>
      t.o.messageInContext(s"This type variable refers to a name that is not actually a type.")
    case OutOfScopeError(use, ref) =>
      use.o.messageInContext(s"This usage is out of scope, ...") + "\n" +
        ref.decl.o.messageInContext("... since it is declared here.")
    // TODO PB: these are kind of obsolete? maybe?
    case IncomparableTypes(left, right) =>
      ???
    case TupleTypeCount(tup) =>
      ???
  }
}
case class TypeError(expr: Expr, expectedType: Type) extends CheckError
case class TypeErrorText(expr: Expr, message: Type => String) extends CheckError
case class GenericTypeError(t: Type, expectedType: TType) extends CheckError
case class OutOfScopeError(use: Node, ref: Ref[_ <: Declaration]) extends CheckError
case class IncomparableTypes(left: Expr, right: Expr) extends CheckError
case class TupleTypeCount(tup: LiteralTuple) extends CheckError

case class CheckContext(scopes: Seq[Set[Declaration]] = Seq(),
                        currentApplicable: Option[Applicable] = None) {
  def withScope(decls: Set[Declaration]): CheckContext =
    CheckContext(scopes :+ decls, currentApplicable)

  def withApplicable(applicable: Applicable): CheckContext =
    CheckContext(scopes, Some(applicable))

  def inScope(ref: Ref[_ <: Declaration]): Boolean =
    scopes.exists(_.contains(ref.decl))

  def checkInScope(use: Node, ref: Ref[_ <: Declaration]): Seq[CheckError] =
    if(inScope(ref))
      Nil
    else
      Seq(OutOfScopeError(use, ref))
}

case class UnreachableAfterTypeCheck(message: String, at: Node) extends ASTStateError {
  override def text: String = "A condition was reached that should have been excluded by the type check. " +
    "Either a property of a node was queried before the type check, or the type check is missing a condition. " +
    f"The node says: $message"
}