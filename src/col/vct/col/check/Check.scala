package vct.col.check

import vct.col.ast._
import vct.col.ast.util.Declarator
import vct.col.err.ASTStateError
import vct.col.origin.Origin
import vct.col.ref.Ref
import vct.col.resolve.ResolveReferences
import vct.col.resolve.ctx.TypeResolutionContext
import vct.result.{HasContext, Message}

import scala.collection.immutable.ListSet
import scala.collection.mutable

case object Check {
  def inOrder(check1: => Seq[CheckError], check2: => Seq[CheckError]): Seq[CheckError] =
    check1 match {
      case Nil => check2
      case more => more
    }
}

sealed trait CheckError {
  def message(context: Node[_] => HasContext): String =
    Message.messagesInContext((this match {
      case TypeError(expr, _) if expr.t.isInstanceOf[TNotAValue[_]] =>
        Seq(context(expr) -> s"This expression is not a value.")
      case TypeErrorText(expr, _) if expr.t.isInstanceOf[TNotAValue[_]] =>
        Seq(context(expr) -> s"This expression is not a value.")
      case TypeError(expr, expectedType) =>
        Seq(context(expr) -> s"Expected the type of this expression to be `$expectedType`, but got ${expr.t}.")
      case TypeErrorText(expr, expectedType) =>
        Seq(context(expr) -> s"Expected the type of this expression to be $expectedType, but got ${expr.t}.")
      case TypeErrorExplanation(expr, message) =>
        Seq(context(expr) -> message)
      case GenericTypeError(t, expectedType) =>
        Seq(context(t) -> s"This type variable refers to a name that is not actually a type.")
      case OutOfScopeError(use, ref) =>
        Seq(context(use) -> "This usage is out of scope,", context(ref.decl) -> "since it is declared here.")
      case ThisOutsideScopeError(use) =>
        Seq(context(use) -> "`this` may not occur outside the declaration it refers to.")
      case ThisInConstructorPre(use) =>
        Seq(context(use) -> "`this` may not occur in the precondition of a constructor.")
      case OutOfWriteScopeError(reason, use, ref) =>
        Seq(
          context(use) -> "This may not be rewritten to, since ...",
          context(reason) -> "declarations outside this node must not be altered, and ...",
          context(ref.decl) -> "... it is declared here.",
        )
      case DoesNotDefine(declarator, declaration, use) =>
        Seq(
          context(use) -> "This uses a declaration, which is declared",
          context(declaration) -> "here, but it was expected to be declared",
          context(declarator.asInstanceOf[Node[_]]) -> "in this declarator.",
        )
      // TODO PB: these are kind of obsolete? maybe?
      case IncomparableTypes(left, right) =>
        ???
      case TupleTypeCount(tup) =>
        ???
      case NotAPredicateApplication(res) =>
        Seq(context(res) -> "This expression is not a (scaled) predicate application")
      case AbstractPredicate(res) =>
        Seq(context(res) -> "This predicate is abstract, and hence cannot be meaningfully folded or unfolded")
      case RedundantCatchClause(clause) =>
        Seq(context(clause) -> "This catch clause is redundant, because it is subsumed by the caught types of earlier catch clauses in this block.")
      case ResultOutsidePostcondition(res) =>
        Seq(context(res) -> "\\result may only occur in the postcondition.")
      case ReturnOutsideMethod(ret) =>
        Seq(context(ret) -> "return may only occur in methods and procedures.")
      case FinalPermission(loc) =>
        Seq(context(loc) -> "Specifying permission over final fields is not allowed, since they are treated as constants.")
      case PVLSeqAssignEndpoint(a) =>
        Seq(context(a) -> "This dereference does not take place on one of the endpoints in the surrounding `seq_prog`.")
      case SeqProgStatement(s) =>
        Seq(context(s) -> "This statement is not allowed in `seq_prog`.")
      case SeqProgInstanceMethodArgs(m) =>
        Seq(context(m) -> "An instance method in a `seq_prog` cannot have any arguments.")
      case SeqProgInstanceMethodBody(m) =>
        Seq(context(m) -> "An instance method in a `seq_prog` must have a body.")
      case SeqProgInstanceMethodNonVoid(m) =>
        Seq(context(m) -> "An instance method in a `seq_prog` must have return type `void`.")
      case SeqProgInvocation(s) =>
        Seq(context(s) -> "Only invocations on `this` and endpoints are allowed.")
      case SeqProgReceivingEndpoint(e) =>
        Seq(context(e) -> s"Can only refer to the receiving endpoint of this statement.")
      case SeqProgParticipant(s) =>
        Seq(context(s) -> s"An endpoint is used in this branch which is not allowed to participate at this point in the program because of earlier branches.")
      case SeqProgNoParticipant(s) =>
        Seq(context(s) -> s"Unclear what the participating endpoint is in this statement")
      case SeqProgEndpointAssign(a) =>
        Seq(context(a) -> s"Raw assignment to an endpoint is not allowed.")
      case SeqProgInstanceMethodPure(m) =>
        Seq(context(m) -> s"Instance methods in seq_programs cannot be pure.")
    }): _*)

  def subcode: String
}

case class TypeError(expr: Expr[_], expectedType: Type[_]) extends CheckError {
  val subcode = "type"
}
case class TypeErrorText(expr: Expr[_], expectedType: String) extends CheckError {
  val subcode = "type"
}
case class TypeErrorExplanation(expr: Node[_], message: String) extends CheckError {
  val subcode = "type"
}
case class GenericTypeError(t: Type[_], expectedType: TType[_]) extends CheckError {
  val subcode = "genericType"
}
case class OutOfScopeError[G](use: Node[G], ref: Ref[G, _ <: Declaration[G]]) extends CheckError {
  val subcode = "outOfScope"
}
case class ThisOutsideScopeError[G](use: ThisDeclaration[G]) extends CheckError {
  override def subcode: String = "thisOutOfScope"
}
case class ThisInConstructorPre[G](use: ThisObject[G]) extends CheckError {
  override def subcode: String = "thisInConsPre"
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
case class ReturnOutsideMethod(ret: Return[_]) extends CheckError {
  val subcode = "resultOutsideMethod"
}
case class FinalPermission(loc: FieldLocation[_]) extends CheckError {
  override def subcode: String = "finalPerm"
}
case class PVLSeqAssignEndpoint(assign: PVLChorStatement[_]) extends CheckError {
  val subcode = "pvlSeqAssignEndpoint"
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
case class SeqProgReceivingEndpoint(e: Expr[_]) extends CheckError {
  val subcode = "seqProgReceivingEndpoint"
}
case class SeqProgParticipant(s: Node[_]) extends CheckError {
  val subcode = "seqProgParticipant"
}
case class SeqProgNoParticipant(s: Node[_]) extends CheckError {
  val subcode = "seqProgNoParticipant"
}
case class SeqProgEndpointAssign(a: Assign[_]) extends CheckError {
  val subcode = "seqProgEndpointAssign"
}
case class SeqProgInstanceMethodPure(m: InstanceMethod[_]) extends CheckError {
  val subcode = "seqProgInstanceMethodPure"
}

case object CheckContext {
  case class ScopeFrame[G](decls: Seq[Declaration[G]], scanLazily: Seq[Node[G]]) {
    private lazy val declSet = decls.toSet
    private lazy val scannedDeclSet = scanLazily.flatMap(ResolveReferences.scanScope(TypeResolutionContext())).toSet

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
  inPreCondition: Boolean = false,
  inPostCondition: Boolean = false,
  currentChoreography: Option[Choreography[G]] = None,
  currentReceiverEndpoint: Option[Endpoint[G]] = None,
  currentParticipatingEndpoints: Option[Set[Endpoint[G]]] = None,
  declarationStack: Seq[Declaration[G]] = Nil,
) {
  def withScope(decls: Seq[Declaration[G]]): Seq[CheckContext.ScopeFrame[G]] =
    scopes :+ CheckContext.ScopeFrame(decls, Nil)

  /**
   * In effect toScan is just scanned for LocalDecl's, and these are added to decls. We want to delay this, because
   * the scanning operation is expensive, and for most of the transformation run the declaration is declared directly
   * anyway.
   */
  def withScope(decls: Seq[Declaration[G]], toScan: Seq[Node[G]]): Seq[CheckContext.ScopeFrame[G]] =
    scopes :+ CheckContext.ScopeFrame(decls, toScan)

  def withDeclaration(decl: Declaration[G]): CheckContext[G] =
    copy(declarationStack = decl +: declarationStack)

  def withApplicable(applicable: Applicable[G]): CheckContext[G] =
    copy(currentApplicable = Some(applicable))

  def withPostcondition: CheckContext[G] =
    copy(inPostCondition = true)

  def withPrecondition: CheckContext[G] =
    copy(inPreCondition = true)

  def withUndeclared(decls: Seq[Declaration[G]]): CheckContext[G] =
    copy(undeclared = undeclared :+ decls)

  def withChoreography(prog: Choreography[G]): CheckContext[G] =
    copy(currentChoreography = Some(prog))

  def withReceiverEndpoint(endpoint: Endpoint[G]): CheckContext[G] =
    copy(currentReceiverEndpoint = Some(endpoint))

  def withCurrentParticipatingEndpoints(endpoints: Seq[Endpoint[G]]): Option[Set[Endpoint[G]]] =
    // ListSet to preserve insertion order
    Some(ListSet.from(endpoints))

  def appendCurrentParticipatingEndpoints(newEndpoints: Seq[Endpoint[G]]): Option[Set[Endpoint[G]]] =
    // ListSet to preserve insertion order
    currentParticipatingEndpoints match {
      case None => withCurrentParticipatingEndpoints(newEndpoints)
      case Some(endpoints) => Some(endpoints.union(ListSet.from(newEndpoints)))
    }

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