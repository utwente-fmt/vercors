package vct.col.ast

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed abstract class Declaration extends Node {
  def succeedDefault(scope: ScopeContext, pred: Declaration): Unit = {
    declareDefault(scope)
    scope.successionMap(pred) = this
  }

  def declareDefault(scope: ScopeContext): Unit
}

object Ref {
  val EXC_MESSAGE = "The AST is in an invalid state: a Ref contains a declaration of the wrong kind."

  def unapply(obj: Any): Option[Declaration] = obj match {
    case ref: Ref => Some(ref.decl)
    case _ => None
  }
}

trait Ref {
  def decl: Declaration

  private def matchOrThrow[T, S](x: T)(f: PartialFunction[T, S]): S =
    f.lift(x).getOrElse(throw new IllegalStateException(Ref.EXC_MESSAGE))

  def asVariable: Variable = matchOrThrow(decl) { case v: Variable => v }
  def asField: Field = matchOrThrow(decl) { case f: Field => f }
  def asSilverField: SilverField = matchOrThrow(decl) { case f: SilverField => f }
  def asApplicable: Applicable = matchOrThrow(decl) { case a: Applicable => a }
  def asFunction: AbstractFunction = matchOrThrow(decl) { case f: AbstractFunction => f }
  def asMethod: AbstractMethod = matchOrThrow(decl) { case m: AbstractMethod => m }
  def asLabel: LabelDecl = matchOrThrow(decl) { case l: LabelDecl => l }

  override def equals(obj: Any): Boolean = obj match {
    case Ref(other) => other eq decl
    case _ => false
  }
}

class DirectRef(val decl: Declaration) extends Ref

class LazyRef(lazyDecl: => Declaration) extends Ref {
  def decl: Declaration = lazyDecl
}

class UnresolvedRef(name: String) extends Ref {
  def decl: Declaration = throw new IllegalStateException("Cannot query an unresolved reference before it is resolved")
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

  def collectInScope[T](scope: mutable.Stack[ArrayBuffer[T]])(f: => Unit): Seq[T] = {
    scope.push(ArrayBuffer())
    f
    scope.pop().toSeq
  }

  def collectOneInScope[T](scope: mutable.Stack[ArrayBuffer[T]])(f: => Unit): T = {
    val result = collectInScope(scope)(f)

    if(result.size != 1) {
      throw new IllegalStateException()
    }

    result.head
  }
}

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

class SimplificationRule(val from: Expr, val to: Expr)(implicit val o: Origin) extends GlobalDeclaration with NoCheck

class AxiomaticDataType(val declarations: Seq[ADTDeclaration], val typeArgs: Seq[Variable])(implicit val o: Origin)
  extends GlobalDeclaration with NoCheck

sealed trait ADTDeclaration extends Declaration {
  override def declareDefault(scope: ScopeContext): Unit = scope.adtScopes.top += this
}
case class ADTAxiom(axiom: Expr)(implicit val o: Origin) extends ADTDeclaration {
  override def check(context: CheckContext): Seq[CheckError] = axiom.checkSubType(TBool())
}


sealed trait Applicable extends Declaration {
  def args: Seq[Variable]
  def returnType: Type
  def body: Option[Node]

  def inline: Boolean
}

sealed trait AbstractPredicate extends Applicable {
  override def body: Option[Expr]
  override def returnType: Type = TResource()
  def threadLocal: Boolean

  override def check(context: CheckContext): Seq[CheckError] = body.toSeq.flatMap(_.checkSubType(TResource()))
}

case class ApplicableContract(requires: Expr, ensures: Expr, contextEverywhere: Expr,
                              signals: Seq[(Variable, Expr)], givenArgs: Seq[Variable], yieldsArgs: Seq[Variable])
                             (implicit val o: Origin)
  extends NodeFamily {
  override def check(context: CheckContext): Seq[CheckError] =
    requires.checkSubType(TResource()) ++
      ensures.checkSubType(TResource()) ++
      contextEverywhere.checkSubType(TResource()) ++
      signals.flatMap(_._2.checkSubType(TResource()))
}

sealed trait ContractApplicable extends Applicable {
  def contract: ApplicableContract
  def blame: PostconditionBlame
}

sealed trait AbstractFunction extends ContractApplicable {
  override def body: Option[Expr]
  override def check(context: CheckContext): Seq[CheckError] = body.toSeq.flatMap(_.checkSubType(returnType))
}

sealed trait AbstractMethod extends ContractApplicable {
  override def body: Option[Statement]
  def outArgs: Seq[Variable]
  def pure: Boolean

  override def check(context: CheckContext): Seq[CheckError] =
    body.toSeq.flatMap(_.transSubnodes.flatMap {
      case Return(e) => e.checkSubType(returnType)
      case _ => Seq()
  })
}

class Function(val returnType: Type, val args: Seq[Variable], val body: Option[Expr], val contract: ApplicableContract, val inline: Boolean = false)
              (val blame: PostconditionBlame)(implicit val o: Origin)
  extends GlobalDeclaration with AbstractFunction

class Procedure(val returnType: Type,
                val args: Seq[Variable], val outArgs: Seq[Variable],
                val body: Option[Statement],
                val contract: ApplicableContract,
                val inline: Boolean = false, val pure: Boolean = false)
               (val blame: PostconditionBlame)(implicit val o: Origin)
  extends GlobalDeclaration with AbstractMethod

class Predicate(val args: Seq[Variable], val body: Option[Expr],
                val threadLocal: Boolean = false, val inline: Boolean = false)(implicit val o: Origin)
  extends GlobalDeclaration with AbstractPredicate

class InstanceFunction(val returnType: Type, val args: Seq[Variable], val body: Option[Expr], val contract: ApplicableContract, val inline: Boolean)
                      (val blame: PostconditionBlame)(implicit val o: Origin)
  extends ClassDeclaration with AbstractFunction

class InstanceMethod(val returnType: Type,
                     val args: Seq[Variable], val outArgs: Seq[Variable],
                     val body: Option[Statement],
                     val contract: ApplicableContract,
                     val inline: Boolean = false, val pure: Boolean = false)
                    (val blame: PostconditionBlame)(implicit val o: Origin)
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
class StaticField(val t: Type, val flags: Set[FieldFlag])(implicit val o: Origin) extends Field with NoCheck

class Class(val declarations: Seq[ClassDeclaration])(implicit val o: Origin) extends GlobalDeclaration with NoCheck

sealed trait ModelDeclaration extends Declaration {
  override def declareDefault(scope: ScopeContext): Unit = scope.modelScopes.top += this
}

class ModelField(val t: Type)(implicit val o: Origin) extends ModelDeclaration with NoCheck
class ModelProcess(val args: Seq[Variable], val impl: Expr,
                   val requires: Expr, val ensures: Expr,
                   val modifies: Seq[Ref], val accessible: Seq[Ref])
                  (implicit val o: Origin) extends ModelDeclaration with Applicable {
  override def returnType: Type = TProcess()
  override def body: Option[Node] = Some(impl)
  override def inline: Boolean = false
  override def check(context: CheckContext): Seq[CheckError] =
    impl.checkSubType(TProcess()) ++ requires.checkSubType(TBool()) ++ ensures.checkSubType(TBool())
}
class ModelAction(val args: Seq[Variable],
                  val requires: Expr, val ensures: Expr,
                  val modifies: Seq[Ref], val accessible: Seq[Ref])
                 (implicit val o: Origin) extends ModelDeclaration with Applicable {
  override def returnType: Type = TProcess()
  override def body: Option[Node] = None
  override def inline: Boolean = false

  override def check(context: CheckContext): Seq[CheckError] =
    requires.checkSubType(TBool()) ++ ensures.checkSubType(TBool())
}

class Model(val declarations: Seq[ModelDeclaration])(implicit val o: Origin) extends GlobalDeclaration with NoCheck