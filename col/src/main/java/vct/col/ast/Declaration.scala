package vct.col.ast

import vct.col.ast.ScopeContext.WrongDeclarationCount
import vct.result.VerificationResult.SystemError

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime

sealed abstract class Declaration extends Node {
  def succeedDefault(scope: ScopeContext, pred: Declaration): Unit = {
    declareDefault(scope)
    scope.successionMap(pred) = this
  }

  def declareDefault(scope: ScopeContext): Unit

  def ref: Ref[this.type] = new DirectRef[this.type](this)
}

object Ref {
  val EXC_MESSAGE = "The AST is in an invalid state: a Ref contains a declaration of the wrong kind."

  def unapply[T <: Declaration](obj: Ref[T]): Option[T] = obj match {
    case ref: Ref[T] => Some(ref.decl)
    case _ => None
  }
}

/* NB: While Ref's can be stricter than just any Declaration (e.g. Ref[Function]), we can construct any variant with
   just a Declaration. This is because:
   - We cannot prove that the successor of a Declaration is of the correct type when we construct it: then it wouldn't
     be lazy and we wouldn't be able to cross-reference declarations out of AST order.
   - We do not want to cast or check refs to be of the correct kind every time we want to use it.
   The most acceptable solution is then to pretend to have a safe interface that returns a declaration of the right
   kind, but quietly check the type on first access.
 */
trait Ref[+T <: Declaration] {
  def decl: T

  def tryResolve(resolver: String => Declaration): Unit = {}

  override def equals(obj: Any): Boolean = obj match {
    case other: Ref[_] => decl == other.decl
  }

  override def hashCode(): Int = ScalaRunTime._hashCode((Ref.getClass, decl))
}

case class MistypedRef(received: Declaration, expected: ClassTag[_]) extends ASTStateError {
  override def text: String =
    "A reference in the AST is referencing a declaration of the wrong kind.\n" +
      s"A ${expected.runtimeClass.getSimpleName} was expected here, but we got a ${received.getClass.getSimpleName}"
}

class DirectRef[+T <: Declaration](genericDecl: Declaration)(implicit tag: ClassTag[T]) extends Ref[T] {
  override def decl: T = genericDecl match {
    case decl: /*tagged*/ T => decl
    case other => throw MistypedRef(other, tag)
  }
}

class LazyRef[+T <: Declaration](lazyDecl: => Declaration)(implicit tag: ClassTag[T]) extends Ref[T] {
  def decl: T = lazyDecl match {
    case decl: /*tagged*/ T => decl
    case other => throw MistypedRef(other, tag)
  }
}

case class NotResolved(ref: UnresolvedRef[Declaration], expected: ClassTag[_]) extends ASTStateError {
  override def text: String =
    "The declaration of an unresolved reference was queried, but it is not yet resolved.\n" +
      s"We expected the name `${ref.name}` to resolve to a ${expected.runtimeClass.getSimpleName}."
}

class UnresolvedRef[+T <: Declaration](val name: String)(implicit tag: ClassTag[T]) extends Ref[T] {
  private var resolvedDecl: Option[Declaration] = None

  override def tryResolve(resolver: String => Declaration): Unit = resolve(resolver(name))

  def resolve(decl: Declaration): Unit = resolvedDecl = Some(decl)

  def decl: T = resolvedDecl match {
    case None => throw NotResolved(this, tag)
    case Some(decl: /*tagged*/ T) => decl
    case Some(other) => throw MistypedRef(other, tag)
  }
}

object ScopeContext {
  case class WrongDeclarationCount(kind: ClassTag[_], count: Int) extends SystemError {
    override def text: String =
      s"Expected exactly one declaration of kind ${kind.runtimeClass.getSimpleName}, but got $count."
  }
}

class ScopeContext {
  // The default action for declarations is to be succeeded by a similar declaration, for example a copy.
  val successionMap: mutable.Map[Declaration, Declaration] = mutable.Map()

  val globalScopes: mutable.Stack[ArrayBuffer[GlobalDeclaration]] = mutable.Stack()
  val classScopes: mutable.Stack[ArrayBuffer[ClassDeclaration]] = mutable.Stack()
  val adtScopes: mutable.Stack[ArrayBuffer[ADTDeclaration]] = mutable.Stack()
  val variableScopes: mutable.Stack[ArrayBuffer[Variable]] = mutable.Stack()
  val labelScopes: mutable.Stack[ArrayBuffer[LabelDecl]] = mutable.Stack()
  val parBlockScopes: mutable.Stack[ArrayBuffer[ParBlockDecl]] = mutable.Stack()
  val parInvariantScopes: mutable.Stack[ArrayBuffer[ParInvariantDecl]] = mutable.Stack()
  val modelScopes: mutable.Stack[ArrayBuffer[ModelDeclaration]] = mutable.Stack()

  val javaLocalScopes: mutable.Stack[ArrayBuffer[JavaLocalDeclaration]] = mutable.Stack()
  val cLocalScopes: mutable.Stack[ArrayBuffer[CDeclaration]] = mutable.Stack()
  val cParams: mutable.Stack[ArrayBuffer[CParam]] = mutable.Stack()

  def collectInScope[T](scope: mutable.Stack[ArrayBuffer[T]])(f: => Unit): Seq[T] = {
    scope.push(ArrayBuffer())
    f
    scope.pop().toSeq
  }

  def collectOneInScope[T](scope: mutable.Stack[ArrayBuffer[T]])(f: => Unit)(implicit tag: ClassTag[T]): T = {
    val result = collectInScope(scope)(f)

    if(result.size != 1) {
      throw WrongDeclarationCount(tag, result.size)
    }

    result.head
  }

  def succ(decl: Declaration): LazyRef[Declaration] =
    new LazyRef[Declaration](successionMap(decl))

  def typedSucc[T <: Declaration](decl: Declaration)(implicit tag: ClassTag[T]): LazyRef[T] =
    new LazyRef[T](successionMap(decl))
}

abstract class ExtraDeclarationKind extends Declaration

sealed abstract class GlobalDeclaration extends Declaration {
  override def declareDefault(scope: ScopeContext): Unit = scope.globalScopes.top += this
}
abstract class ExtraGlobalDeclaration extends GlobalDeclaration

sealed abstract class ClassDeclaration extends Declaration {
  override def declareDefault(scope: ScopeContext): Unit = scope.classScopes.top += this
}
abstract class ExtraClassDeclaration extends ClassDeclaration

/* Common type for unit names: locals, bindings, arguments, etc. (but not fields, as they are only in reference to an
  object) */
class Variable(val t: Type)(implicit val o: Origin) extends Declaration with NoCheck {
  override def declareDefault(scope: ScopeContext): Unit = scope.variableScopes.top += this
}

class LabelDecl()(implicit val o: Origin) extends Declaration with NoCheck {
  override def declareDefault(scope: ScopeContext): Unit = scope.labelScopes.top += this
}
class ParBlockDecl()(implicit val o: Origin) extends Declaration with NoCheck {
  override def declareDefault(scope: ScopeContext): Unit = scope.parBlockScopes.top += this
}
class ParInvariantDecl()(implicit val o: Origin) extends Declaration with NoCheck {
  override def declareDefault(scope: ScopeContext): Unit = scope.parInvariantScopes.top += this
}

class SimplificationRule(val axiom: Expr)(implicit val o: Origin) extends GlobalDeclaration with NoCheck

class AxiomaticDataType(val decls: Seq[ADTDeclaration], val typeArgs: Seq[Variable])(implicit val o: Origin)
  extends GlobalDeclaration with NoCheck with Declarator {
  override def declarations: Seq[Declaration] = decls ++ typeArgs
}

sealed trait ADTDeclaration extends Declaration {
  override def declareDefault(scope: ScopeContext): Unit = scope.adtScopes.top += this
}
case class ADTAxiom(axiom: Expr)(implicit val o: Origin) extends ADTDeclaration {
  override def check(context: CheckContext): Seq[CheckError] = axiom.checkSubType(TBool())
}


sealed trait Applicable extends Declaration with Declarator {
  def args: Seq[Variable]
  def returnType: Type
  def body: Option[Node]
  def inline: Boolean

  override def declarations: Seq[Declaration] = args

  override def enterCheckContext(context: CheckContext): CheckContext = context.withApplicable(this)
}

sealed trait AbstractPredicate extends Applicable {
  override def body: Option[Expr]
  override def returnType: Type = TResource()
  def threadLocal: Boolean

  override def check(context: CheckContext): Seq[CheckError] = body.toSeq.flatMap(_.checkSubType(TResource()))
}

case class SignalsClause(binding: Variable, assn: Expr)(implicit val o: Origin) extends Check(assn.checkSubType(TResource())) with NodeFamily with Declarator {
  override def declarations: Seq[Declaration] = Seq(binding)
}

case class ApplicableContract(requires: Expr, ensures: Expr, contextEverywhere: Expr,
                              signals: Seq[SignalsClause], givenArgs: Seq[Variable], yieldsArgs: Seq[Variable])
                             (implicit val o: Origin)
  extends NodeFamily {
  override def check(context: CheckContext): Seq[CheckError] =
    requires.checkSubType(TResource()) ++
      ensures.checkSubType(TResource()) ++
      contextEverywhere.checkSubType(TResource())
}

sealed trait ContractApplicable extends Applicable {
  def contract: ApplicableContract
  def blame: Blame[PostconditionFailed]
  override def declarations: Seq[Declaration] =
    super.declarations ++ contract.givenArgs ++ contract.yieldsArgs
}

sealed trait AbstractFunction extends ContractApplicable {
  override def body: Option[Expr]
  override def check(context: CheckContext): Seq[CheckError] = body.toSeq.flatMap(_.checkSubType(returnType))
}

sealed trait AbstractMethod extends ContractApplicable {
  override def body: Option[Statement]
  def outArgs: Seq[Variable]
  def pure: Boolean

  override def declarations: Seq[Declaration] = super.declarations ++ outArgs

  override def check(context: CheckContext): Seq[CheckError] =
    body.toSeq.flatMap(_.transSubnodes.flatMap {
      case Return(e) => e.checkSubType(returnType)
      case _ => Seq()
  })
}

class Function(val returnType: Type, val args: Seq[Variable], val body: Option[Expr], val contract: ApplicableContract, val inline: Boolean = false)
              (val blame: Blame[PostconditionFailed])(implicit val o: Origin)
  extends GlobalDeclaration with AbstractFunction

class Procedure(val returnType: Type,
                val args: Seq[Variable], val outArgs: Seq[Variable],
                val body: Option[Statement],
                val contract: ApplicableContract,
                val inline: Boolean = false, val pure: Boolean = false)
               (val blame: Blame[PostconditionFailed])(implicit val o: Origin)
  extends GlobalDeclaration with AbstractMethod

class Predicate(val args: Seq[Variable], val body: Option[Expr],
                val threadLocal: Boolean = false, val inline: Boolean = false)(implicit val o: Origin)
  extends GlobalDeclaration with AbstractPredicate

class InstanceFunction(val returnType: Type, val args: Seq[Variable], val body: Option[Expr], val contract: ApplicableContract, val inline: Boolean)
                      (val blame: Blame[PostconditionFailed])(implicit val o: Origin)
  extends ClassDeclaration with AbstractFunction

class InstanceMethod(val returnType: Type,
                     val args: Seq[Variable], val outArgs: Seq[Variable],
                     val body: Option[Statement],
                     val contract: ApplicableContract,
                     val inline: Boolean = false, val pure: Boolean = false)
                    (val blame: Blame[PostconditionFailed])(implicit val o: Origin)
  extends ClassDeclaration with AbstractMethod

class InstancePredicate(val args: Seq[Variable], val body: Option[Expr],
                        val threadLocal: Boolean = false, val inline: Boolean = false)(implicit val o: Origin)
  extends ClassDeclaration with AbstractPredicate

class ADTFunction(val args: Seq[Variable], val returnType: Type)(implicit val o: Origin) extends Applicable with ADTDeclaration with NoCheck {
  override def body: Option[Node] = None
  override def inline: Boolean = false
}

sealed trait FieldFlag extends NodeFamily with NoCheck
class Final()(implicit val o: Origin) extends FieldFlag

sealed trait Field extends ClassDeclaration {
  def t: Type
}

class InstanceField(val t: Type, val flags: Set[FieldFlag])(implicit val o: Origin) extends Field with NoCheck

class Class(val declarations: Seq[ClassDeclaration], val supports: Seq[Ref[Class]])(implicit val o: Origin) extends GlobalDeclaration with NoCheck with Declarator

sealed trait ModelDeclaration extends Declaration {
  override def declareDefault(scope: ScopeContext): Unit = scope.modelScopes.top += this
}

class ModelField(val t: Type)(implicit val o: Origin) extends ModelDeclaration with NoCheck
class ModelProcess(val args: Seq[Variable], val impl: Expr,
                   val requires: Expr, val ensures: Expr,
                   val modifies: Seq[Ref[ModelField]], val accessible: Seq[Ref[ModelField]])
                  (implicit val o: Origin) extends ModelDeclaration with Applicable {
  override def returnType: Type = TProcess()
  override def body: Option[Node] = Some(impl)
  override def inline: Boolean = false
  override def check(context: CheckContext): Seq[CheckError] =
    impl.checkSubType(TProcess()) ++ requires.checkSubType(TBool()) ++ ensures.checkSubType(TBool())
}
class ModelAction(val args: Seq[Variable],
                  val requires: Expr, val ensures: Expr,
                  val modifies: Seq[Ref[ModelField]], val accessible: Seq[Ref[ModelField]])
                 (implicit val o: Origin) extends ModelDeclaration with Applicable {
  override def returnType: Type = TProcess()
  override def body: Option[Node] = None
  override def inline: Boolean = false

  override def check(context: CheckContext): Seq[CheckError] =
    requires.checkSubType(TBool()) ++ ensures.checkSubType(TBool())
}

class Model(val declarations: Seq[ModelDeclaration])(implicit val o: Origin) extends GlobalDeclaration with NoCheck with Declarator