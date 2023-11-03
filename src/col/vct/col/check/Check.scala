package vct.col.check

import vct.col.ast._
import vct.col.ast.util.Declarator
import vct.col.err.ASTStateError
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.resolve.ResolveReferences

case object Check {
  def inOrder(check1: => Seq[CheckError], check2: => Seq[CheckError]): Seq[CheckError] =
    check1 match {
      case Nil => check2
      case more => more
    }
}

sealed trait CheckError {
  def message(context: (Node[_], String) => String): String = (this match {
    case TypeError(expr, _) if expr.t.isInstanceOf[TNotAValue[_]] =>
      Seq(context(expr, s"This expression is not a value."))
    case TypeErrorText(expr, _) if expr.t.isInstanceOf[TNotAValue[_]] =>
      Seq(context(expr, s"This expression is not a value."))
    case TypeError(expr, expectedType) =>
      Seq(context(expr, s"Expected the type of this expression to be `$expectedType`, but got ${expr.t}."))
    case TypeErrorText(expr, expectedType) =>
      Seq(context(expr, s"Expected the type of this expression to be $expectedType, but got ${expr.t}."))
    case TypeErrorExplanation(expr, message) =>
      Seq(context(expr, message))
    case GenericTypeError(t, expectedType) =>
      Seq(context(t, s"This type variable refers to a name that is not actually a type."))
    case OutOfScopeError(use, ref) =>
      Seq(context(use, "This usage is out of scope,"), context(ref.decl, "since it is declared here."))
    case OutOfWriteScopeError(reason, use, ref) =>
      Seq(
        context(use, "This may not be rewritten to, since ..."),
        context(reason, "declarations outside this node must not be altered, and ..."),
        context(ref.decl, "... it is declared here."),
      )
    case DoesNotDefine(declarator, declaration, use) =>
      Seq(
        context(use, "This uses a declaration, which is declared"),
        context(declaration, "here, but it was expected to be declared"),
        context(declarator.asInstanceOf[Node[_]], "in this declarator."),
      )
    // TODO PB: these are kind of obsolete? maybe?
    case IncomparableTypes(left, right) =>
      ???
    case TupleTypeCount(tup) =>
      ???
    case NotAPredicateApplication(res) =>
      Seq(context(res, "This expression is not a (scaled) predicate application"))
    case AbstractPredicate(res) =>
      Seq(context(res, "This predicate is abstract, and hence cannot be meaningfully folded or unfolded"))
    case RedundantCatchClause(clause) =>
      Seq(context(clause, "This catch clause is redundant, because it is subsumed by the caught types of earlier catch clauses in this block."))
    case ResultOutsidePostcondition(res) =>
      Seq(context(res, "\\result may only occur in the postcondition."))
    case SeqProgStatement(s) => Seq(context(s, "This statement is not allowed in `seq_prog`."))
    case SeqProgInstanceMethodArgs(m) => Seq(context(m, "An instance method in a `seq_prog` cannot have any arguments."))
    case SeqProgInstanceMethodBody(m) => Seq(context(m, "An instance method in a `seq_prog` must have a body."))
    case SeqProgInstanceMethodNonVoid(m) => Seq(context(m, "An instance method in a `seq_prog` must have return type `void`."))
    case SeqProgInvocation(s) => Seq(context(s, "Only invocations on `this` and endpoints are allowed."))
    case SeqProgInvocationReceiver(e) => Seq(context(e, "Referring to other endpoints within an invocation on a specific endpoint is not yet supported."))
  }).mkString(Origin.BOLD_HR, Origin.HR, Origin.BOLD_HR)

  def subcode: String
}

case class TypeError(expr: Expr[_], expectedType: Type[_]) extends CheckError {
  val subcode = "type"
}
case class TypeErrorText(expr: Expr[_], expectedType: String) extends CheckError {
  val subcode = "type"
}
case class TypeErrorExplanation(expr: Expr[_], message: String) extends CheckError {
  val subcode = "type"
}
case class GenericTypeError(t: Type[_], expectedType: TType[_]) extends CheckError {
  val subcode = "genericType"
}
case class OutOfScopeError[G](use: Node[G], ref: Ref[G, _ <: Declaration[G]]) extends CheckError {
  val subcode = "outOfScope"
}
case class OutOfWriteScopeError[G](reason: Node[G], use: Node[G], ref: Ref[G, _ <: Declaration[G]]) extends CheckError {
  val subcode = "outOfWriteScope"
}
case class DoesNotDefine(declarator: Declarator[_], declaration: Declaration[_], use: Node[_]) extends CheckError {
  val subcode = "doesNotDefine"
}
case class IncomparableTypes(left: Expr[_], right: Expr[_]) extends CheckError {
  val subcode = "incomparableTypes"
}
case class TupleTypeCount(tup: LiteralTuple[_]) extends CheckError {
  val subcode = "tupleTypeCount"
}
case class NotAPredicateApplication(res: Expr[_]) extends CheckError {
  val subcode = "notAPredicateApplication"
}
case class AbstractPredicate(res: Expr[_]) extends CheckError {
  val subcode = "abstractPredicate"
}
case class RedundantCatchClause(clause: CatchClause[_]) extends CheckError {
  val subcode = "redundantCatchClause"
}
case class ResultOutsidePostcondition(res: Expr[_]) extends CheckError {
  val subcode = "resultOutsidePostcondition"
}
case class SeqProgInstanceMethodNonVoid(m: InstanceMethod[_]) extends CheckError {
  val subcode = "seqProgInstanceMethodNonVoid"
}
case class SeqProgInstanceMethodArgs(m: InstanceMethod[_]) extends CheckError {
  val subcode = "seqProgInstanceMethodArgs"
}
case class SeqProgInstanceMethodBody(m: InstanceMethod[_]) extends CheckError {
  val subcode = "seqProgInstanceMethodBody"
}
case class SeqProgStatement(s: Statement[_]) extends CheckError {
  val subcode = "seqProgStatement"
}
case class SeqProgInvocation(s: Statement[_]) extends CheckError {
  val subcode = "seqProgInvocation"
}
case class SeqProgInvocationReceiver(e: Expr[_]) extends CheckError {
  val subcode = "seqProgInvocationReceiver"
}

case object CheckContext {
  case class ScopeFrame[G](decls: Seq[Declaration[G]], scanLazily: Seq[Node[G]]) {
    private lazy val declSet = decls.toSet
    private lazy val scannedDeclSet = scanLazily.flatMap(ResolveReferences.scanScope(_, inGPUKernel = false)).toSet

    def contains(decl: Declaration[G]): Boolean =
      declSet.contains(decl) || (scanLazily.nonEmpty && scannedDeclSet.contains(decl))
  }
}

case class CheckContext[G]
(
  scopes: Seq[CheckContext.ScopeFrame[G]] = Seq(),
  undeclared: Seq[Seq[Declaration[G]]] = Nil,
  roScopes: Int = 0, roScopeReason: Option[Node[G]] = None,
  currentApplicable: Option[Applicable[G]] = None,
  inPostCondition: Boolean = false,
  currentSeqProg: Option[SeqProg[G]] = None,
  currentReceiverEndpoint: Option[Endpoint[G]] = None,
) {
  def withScope(decls: Seq[Declaration[G]]): CheckContext[G] =
    copy(scopes = scopes :+ CheckContext.ScopeFrame(decls, Nil))

  /**
   * In effect toScan is just scanned for LocalDecl's, and these are added to decls. We want to delay this, because
   * the scanning operation is expensive, and for most of the transformation run the declaration is declared directly
   * anyway.
   */
  def withScope(decls: Seq[Declaration[G]], toScan: Seq[Node[G]]): CheckContext[G] =
    copy(scopes = scopes :+ CheckContext.ScopeFrame(decls, toScan))

  def withApplicable(applicable: Applicable[G]): CheckContext[G] =
    copy(currentApplicable = Some(applicable))

  def withPostcondition: CheckContext[G] =
    copy(inPostCondition = true)

  def withUndeclared(decls: Seq[Declaration[G]]): CheckContext[G] =
    copy(undeclared = undeclared :+ decls)

  def withSeqProg(prog: SeqProg[G]): CheckContext[G] =
    copy(currentSeqProg = Some(prog))

  def withReceiverEndpoint(endpoint: Endpoint[G]): CheckContext[G] =
    copy(currentReceiverEndpoint = Some(endpoint))

  def inScope[Decl <: Declaration[G]](ref: Ref[G, Decl]): Boolean =
    !undeclared.exists(_.contains(ref.decl)) && scopes.exists(_.contains(ref.decl))

  def inWriteScope[Decl <: Declaration[G]](ref: Ref[G, Decl]): Boolean =
    !undeclared.exists(_.contains(ref.decl)) && scopes.drop(roScopes).exists(_.contains(ref.decl))

  def checkInScope[Decl <: Declaration[G]](use: Node[G], ref: Ref[G, Decl]): Seq[CheckError] =
    if(inScope(ref)) Nil
    else Seq(OutOfScopeError(use, ref))

  def checkInWriteScope[Decl <: Declaration[G]](reason: Option[Node[G]], use: Node[G], ref: Ref[G, Decl]): Seq[CheckError] =
    if (!inScope(ref)) Seq(OutOfScopeError(use, ref))
    else if(!inWriteScope(ref)) Seq(OutOfWriteScopeError(reason.get, use, ref))
    else Nil
}

case class UnreachableAfterTypeCheck(message: String, at: Node[_]) extends ASTStateError {
  override def text: String = "A condition was reached that should have been excluded by the type check. " +
    "Either a property of a node was queried before the type check, or the type check is missing a condition. " +
    f"The node says: $message"
}