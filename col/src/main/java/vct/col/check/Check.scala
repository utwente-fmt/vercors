package vct.col.check

import vct.col.ast._
import vct.col.ast.util.Declarator
import vct.col.err.ASTStateError
import vct.col.origin.Origin
import vct.col.ref.Ref

case object Check {
  def inOrder(check1: => Seq[CheckError], check2: => Seq[CheckError]): Seq[CheckError] =
    check1 match {
      case Nil => check2
      case more => more
    }
}

sealed trait CheckError {
  override def toString: String = this match {
    case TypeError(expr, expectedType) =>
      expr.o.messageInContext(s"Expected the type of this expression to be `$expectedType`, but got ${expr.t}.")
    case TypeErrorText(expr, message) =>
      expr.o.messageInContext(message(expr.t))
    case GenericTypeError(t, expectedType) =>
      t.o.messageInContext(s"This type variable refers to a name that is not actually a type.")
    case OutOfScopeError(use, ref) =>
      Origin.messagesInContext(Seq(
        (use.o, "This usage is out of scope,"),
        (ref.decl.o, "since it is declared here.")
      ))
    case DoesNotDefine(declarator, declaration, use) =>
      Origin.messagesInContext(Seq(
        (use.o, "This uses a declaration, which is declared"),
        (declaration.o, "here, but it was expected to be declared"),
        (declarator.o, "in this declarator."),
      ))
    // TODO PB: these are kind of obsolete? maybe?
    case IncomparableTypes(left, right) =>
      ???
    case TupleTypeCount(tup) =>
      ???
    case NotAPredicateApplication(res) =>
      res.o.messageInContext("This expression is not a (scaled) predicate application")
    case AbstractPredicate(res) =>
      res.o.messageInContext("This predicate is abstract, and hence cannot be meaningfully folded or unfolded")
    case RedundantCatchClause(clause) =>
      clause.o.messageInContext("This catch clause is redundant, because it is subsumed by the caught types of earlier catch clauses in this block.")
    case ResultOutsidePostcondition(res) =>
      res.o.messageInContext("\\result may only occur in the postcondition.")
  }
}
case class TypeError(expr: Expr[_], expectedType: Type[_]) extends CheckError
case class TypeErrorText(expr: Expr[_], message: Type[_] => String) extends CheckError
case class GenericTypeError(t: Type[_], expectedType: TType[_]) extends CheckError
case class OutOfScopeError[G](use: Node[G], ref: Ref[G, _ <: Declaration[G]]) extends CheckError
case class DoesNotDefine(declarator: Declarator[_], declaration: Declaration[_], use: Node[_]) extends CheckError
case class IncomparableTypes(left: Expr[_], right: Expr[_]) extends CheckError
case class TupleTypeCount(tup: LiteralTuple[_]) extends CheckError
case class NotAPredicateApplication(res: Expr[_]) extends CheckError
case class AbstractPredicate(res: Expr[_]) extends CheckError
case class RedundantCatchClause(clause: CatchClause[_]) extends CheckError
case class ResultOutsidePostcondition(res: Result[_]) extends CheckError

case class CheckContext[G]
(
  scopes: Seq[Set[Declaration[G]]] = Seq(),
  currentApplicable: Option[Applicable[G]] = None,
  inPostCondition: Boolean = false,
) {
  def withScope(decls: Set[Declaration[G]]): CheckContext[G] =
    copy(scopes = scopes :+ decls)

  def withApplicable(applicable: Applicable[G]): CheckContext[G] =
    copy(currentApplicable = Some(applicable))

  def withPostcondition: CheckContext[G] =
    copy(inPostCondition = true)

  def inScope[Decl <: Declaration[G]](ref: Ref[G, Decl]): Boolean =
    scopes.exists(_.contains(ref.decl))

  def checkInScope[Decl <: Declaration[G]](use: Node[G], ref: Ref[G, Decl]): Seq[CheckError] =
    if(inScope(ref))
      Nil
    else
      Seq(OutOfScopeError(use, ref))
}

case class UnreachableAfterTypeCheck(message: String, at: Node[_]) extends ASTStateError {
  override def text: String = "A condition was reached that should have been excluded by the type check. " +
    "Either a property of a node was queried before the type check, or the type check is missing a condition. " +
    f"The node says: $message"
}