package vct.col.ast

import vct.col.ast.temporaryimplpackage.`type`._
import vct.col.ast.temporaryimplpackage.`type`.typeclass._
import vct.col.ast.temporaryimplpackage.declaration._
import vct.col.ast.temporaryimplpackage.declaration.adt._
import vct.col.ast.temporaryimplpackage.declaration.category._
import vct.col.ast.temporaryimplpackage.declaration.cls._
import vct.col.ast.temporaryimplpackage.declaration.global._
import vct.col.ast.temporaryimplpackage.declaration.model._
import vct.col.ast.temporaryimplpackage.declaration.singular._
import vct.col.ast.temporaryimplpackage.expr._
import vct.col.ast.temporaryimplpackage.expr.`type`._
import vct.col.ast.temporaryimplpackage.expr.ambiguous._
import vct.col.ast.temporaryimplpackage.expr.apply._
import vct.col.ast.temporaryimplpackage.expr.binder._
import vct.col.ast.temporaryimplpackage.family.coercion._
import vct.col.ast.temporaryimplpackage.expr.context._
import vct.col.ast.temporaryimplpackage.expr.heap.alloc._
import vct.col.ast.temporaryimplpackage.expr.heap.read._
import vct.col.ast.temporaryimplpackage.expr.literal.build._
import vct.col.ast.temporaryimplpackage.expr.literal.constant._
import vct.col.ast.temporaryimplpackage.expr.lock._
import vct.col.ast.temporaryimplpackage.expr.misc._
import vct.col.ast.temporaryimplpackage.expr.model._
import vct.col.ast.temporaryimplpackage.expr.op._
import vct.col.ast.temporaryimplpackage.expr.op.bit._
import vct.col.ast.temporaryimplpackage.expr.op.bool._
import vct.col.ast.temporaryimplpackage.expr.op.cmp._
import vct.col.ast.temporaryimplpackage.expr.op.collection._
import vct.col.ast.temporaryimplpackage.expr.op.either._
import vct.col.ast.temporaryimplpackage.expr.op.map._
import vct.col.ast.temporaryimplpackage.expr.op.num._
import vct.col.ast.temporaryimplpackage.expr.op.option._
import vct.col.ast.temporaryimplpackage.expr.op.process._
import vct.col.ast.temporaryimplpackage.expr.op.tuple._
import vct.col.ast.temporaryimplpackage.expr.op.vec._
import vct.col.ast.temporaryimplpackage.expr.resource._
import vct.col.ast.temporaryimplpackage.expr.sideeffect._
import vct.col.ast.temporaryimplpackage.family.accountedpredicate._
import vct.col.ast.temporaryimplpackage.family.catchclause._
import vct.col.ast.temporaryimplpackage.family.coercion._
import vct.col.ast.temporaryimplpackage.family.contract._
import vct.col.ast.temporaryimplpackage.family.fieldflag._
import vct.col.ast.temporaryimplpackage.family.invoking._
import vct.col.ast.temporaryimplpackage.family.itervariable._
import vct.col.ast.temporaryimplpackage.family.javavar.JavaVariableDeclarationImpl
import vct.col.ast.temporaryimplpackage.family.loopcontract._
import vct.col.ast.temporaryimplpackage.family.parregion._
import vct.col.ast.temporaryimplpackage.family.signals._
import vct.col.ast.temporaryimplpackage.lang._
import vct.col.ast.temporaryimplpackage.node._
import vct.col.ast.temporaryimplpackage.statement._
import vct.col.ast.temporaryimplpackage.statement.composite._
import vct.col.ast.temporaryimplpackage.statement.exceptional._
import vct.col.ast.temporaryimplpackage.statement.nonexecutable._
import vct.col.ast.temporaryimplpackage.statement.terminal._
import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.debug._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.resolve._

sealed trait Node[G] extends NodeImpl[G]

sealed trait NodeFamily[G] extends Node[G] with NodeFamilyImpl[G]

final case class Program[G](declarations: Seq[GlobalDeclaration[G]], rootClass: Option[Type[G]])(val blame: Blame[UnsafeCoercion])(implicit val o: Origin) extends NodeFamily[G] with ProgramImpl[G]

sealed trait Type[G] extends NodeFamily[G] with TypeImpl[G]

object TNotAValue {
  def unappply[G](t: TNotAValue[G]): Some[Referrable[G]] = Some(t.decl.get)
}

final class TNotAValue[G]()(implicit val o: Origin = DiagnosticOrigin) extends Type[G] with TNotAValueImpl[G] {
  var decl: Option[Referrable[G]] = None
}
final case class TUnion[G](types: Seq[Type[G]])(implicit val o: Origin = DiagnosticOrigin) extends Type[G] with TUnionImpl[G]
final case class TArray[G](element: Type[G])(implicit val o: Origin = DiagnosticOrigin) extends Type[G] with TArrayImpl[G]
final case class TPointer[G](element: Type[G])(implicit val o: Origin = DiagnosticOrigin) extends Type[G] with TPointerImpl[G]
final case class TType[G](t: Type[G])(implicit val o: Origin = DiagnosticOrigin) extends Type[G] with TTypeImpl[G]
final case class TVar[G](ref: Ref[G, Variable[G]])(implicit val o: Origin = DiagnosticOrigin) extends Type[G] with TVarImpl[G]

sealed trait CompositeType[G] extends Type[G] with CompositeTypeImpl[G]
sealed trait SizedType[G] extends CompositeType[G] with SizedTypeImpl[G]
final case class TSeq[G](element: Type[G])(implicit val o: Origin = DiagnosticOrigin) extends SizedType[G] with TSeqImpl[G]
final case class TSet[G](element: Type[G])(implicit val o: Origin = DiagnosticOrigin) extends SizedType[G] with TSetImpl[G]
final case class TBag[G](element: Type[G])(implicit val o: Origin = DiagnosticOrigin) extends SizedType[G] with TBagImpl[G]
final case class TMap[G](key: Type[G], value: Type[G])(implicit val o: Origin = DiagnosticOrigin) extends SizedType[G] with TMapImpl[G]

final case class TOption[G](element: Type[G])(implicit val o: Origin = DiagnosticOrigin) extends CompositeType[G] with TOptionImpl[G]
final case class TTuple[G](elements: Seq[Type[G]])(implicit val o: Origin = DiagnosticOrigin) extends CompositeType[G] with TTupleImpl[G]
final case class TEither[G](left: Type[G], right: Type[G])(implicit val o: Origin = DiagnosticOrigin) extends CompositeType[G] with TEitherImpl[G]
final case class TMatrix[G](element: Type[G])(implicit val o: Origin = DiagnosticOrigin) extends CompositeType[G] with TMatrixImpl[G]

sealed trait PrimitiveType[G] extends Type[G] with PrimitiveTypeImpl[G]
final case class TAny[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TAnyImpl[G]
final case class TNothing[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TNothingImpl[G]
final case class TVoid[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TVoidImpl[G]
final case class TNull[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TNullImpl[G]
final case class TBool[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TBoolImpl[G]
final case class TResource[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TResourceImpl[G]
final case class TChar[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TCharImpl[G]
final case class TString[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TStringImpl[G]
final case class TRef[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TRefImpl[G]
final case class TProcess[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TProcessImpl[G]

sealed trait NumericType[G] extends PrimitiveType[G] with NumericTypeImpl[G]
final case class TInt[G]()(implicit val o: Origin = DiagnosticOrigin) extends NumericType[G] with TIntImpl[G]
final case class TBoundedInt[G](gte: BigInt, lt: BigInt)(implicit val o: Origin = DiagnosticOrigin) extends NumericType[G] with TBoundedIntImpl[G]
final case class TFloat[G]()(implicit val o: Origin = DiagnosticOrigin) extends NumericType[G] with TFloatImpl[G]
final case class TRational[G]()(implicit val o: Origin = DiagnosticOrigin) extends NumericType[G] with TRationalImpl[G]
final case class TFraction[G]()(implicit val o: Origin = DiagnosticOrigin) extends NumericType[G] with TFractionImpl[G]
final case class TZFraction[G]()(implicit val o: Origin = DiagnosticOrigin) extends NumericType[G] with TZFractionImpl[G]

sealed trait DeclaredType[G] extends Type[G] with DeclaredTypeImpl[G]
final case class TModel[G](model: Ref[G, Model[G]])(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType[G] with TModelImpl[G]
final case class TClass[G](cls: Ref[G, Class[G]])(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType[G] with TClassImpl[G]
final case class TAxiomatic[G](adt: Ref[G, AxiomaticDataType[G]], args: Seq[Type[G]])(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType[G] with TAxiomaticImpl[G]

sealed trait ParRegion[G] extends NodeFamily[G] with ParRegionImpl[G]
final case class ParParallel[G](regions: Seq[ParRegion[G]])(val blame: Blame[ParPreconditionFailed])(implicit val o: Origin) extends ParRegion[G] with ParParallelImpl[G]
final case class ParSequential[G](regions: Seq[ParRegion[G]])(val blame: Blame[ParPreconditionFailed])(implicit val o: Origin) extends ParRegion[G] with ParSequentialImpl[G]
final case class ParBlock[G](decl: ParBlockDecl[G], iters: Seq[IterVariable[G]], context_everywhere: Expr[G], requires: Expr[G], ensures: Expr[G], content: Statement[G])(val blame: Blame[ParBlockFailure])(implicit val o: Origin) extends ParRegion[G] with ParBlockImpl[G]

sealed trait LoopContract[G] extends NodeFamily[G] with LoopContractImpl[G]
final case class LoopInvariant[G](invariant: Expr[G])(val blame: Blame[LoopInvariantFailure])(implicit val o: Origin) extends LoopContract[G] with LoopInvariantImpl[G]
final case class IterationContract[G](requires: Expr[G], ensures: Expr[G], context_everywhere: Expr[G])(val blame: Blame[ParBlockFailure])(implicit val o: Origin) extends LoopContract[G] with IterationContractImpl[G]

final case class CatchClause[G](decl: Variable[G], body: Statement[G])(implicit val o: Origin) extends NodeFamily[G] with CatchClauseImpl[G]

final case class IterVariable[G](variable: Variable[G], from: Expr[G], to: Expr[G])(implicit val o: Origin) extends NodeFamily[G] with IterVariableImpl[G]

sealed trait Statement[G] extends NodeFamily[G] with StatementImpl[G]

sealed trait NonExecutableStatement[G] extends Statement[G] with NonExecutableStatementImpl[G]
final case class LocalDecl[G](local: Variable[G])(implicit val o: Origin) extends NonExecutableStatement[G] with LocalDeclImpl[G]
final case class SpecIgnoreStart[G]()(implicit val o: Origin) extends NonExecutableStatement[G] with SpecIgnoreStartImpl[G]
final case class SpecIgnoreEnd[G]()(implicit val o: Origin) extends NonExecutableStatement[G] with SpecIgnoreEndImpl[G]

sealed trait NormallyCompletingStatement[G] extends Statement[G] with NormallyCompletingStatementImpl[G]
final case class Assign[G](target: Expr[G], value: Expr[G])(val blame: Blame[AssignFailed])(implicit val o: Origin) extends NormallyCompletingStatement[G] with AssignImpl[G]
final case class Send[G](decl: SendDecl[G], delta: BigInt, res: Expr[G])(val blame: Blame[SendFailed])(implicit val o: Origin) extends NormallyCompletingStatement[G] with SendImpl[G]
final case class Recv[G](ref: Ref[G, SendDecl[G]])(implicit val o: Origin) extends NormallyCompletingStatement[G] with RecvImpl[G]
sealed trait SwitchCase[G] extends NormallyCompletingStatement[G] with SwitchCaseImpl[G]
final case class DefaultCase[G]()(implicit val o: Origin) extends SwitchCase[G] with DefaultCaseImpl[G]
final case class Case[G](pattern: Expr[G])(implicit val o: Origin) extends SwitchCase[G] with CaseImpl[G]
final case class Label[G](decl: LabelDecl[G], stat: Statement[G])(implicit val o: Origin) extends NormallyCompletingStatement[G] with LabelImpl[G]
final case class Goto[G](lbl: Ref[G, LabelDecl[G]])(implicit val o: Origin) extends NormallyCompletingStatement[G] with GotoImpl[G]
final case class Exhale[G](res: Expr[G])(val blame: Blame[ExhaleFailed])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExhaleImpl[G]
final case class Assert[G](res: Expr[G])(val blame: Blame[AssertFailed])(implicit val o: Origin) extends NormallyCompletingStatement[G] with AssertImpl[G]
final case class Refute[G](assn: Expr[G])(implicit val o: Origin) extends NormallyCompletingStatement[G] with RefuteImpl[G]
final case class Inhale[G](res: Expr[G])(implicit val o: Origin) extends NormallyCompletingStatement[G] with InhaleImpl[G]
final case class Assume[G](assn: Expr[G])(implicit val o: Origin) extends NormallyCompletingStatement[G] with AssumeImpl[G]
final case class Wait[G](obj: Expr[G])(val blame: Blame[UnlockFailure])(implicit val o: Origin) extends NormallyCompletingStatement[G] with WaitImpl[G]
final case class Notify[G](obj: Expr[G])(val blame: Blame[NotifyFailed])(implicit val o: Origin) extends NormallyCompletingStatement[G] with NotifyImpl[G]
final case class Fork[G](obj: Expr[G])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ForkImpl[G]
final case class Join[G](obj: Expr[G])(implicit val o: Origin) extends NormallyCompletingStatement[G] with JoinImpl[G]
final case class Lock[G](obj: Expr[G])(implicit val o: Origin) extends NormallyCompletingStatement[G] with LockImpl[G]
final case class Unlock[G](obj: Expr[G])(val blame: Blame[UnlockFailure])(implicit val o: Origin) extends NormallyCompletingStatement[G] with UnlockImpl[G]
final case class Commit[G](obj: Expr[G])(val blame: Blame[CommitFailed])(implicit val o: Origin) extends NormallyCompletingStatement[G] with CommitImpl[G]
final case class Fold[G](res: Expr[G])(val blame: Blame[FoldFailed])(implicit val o: Origin) extends NormallyCompletingStatement[G] with FoldImpl[G]
final case class Unfold[G](res: Expr[G])(val blame: Blame[UnfoldFailed])(implicit val o: Origin) extends NormallyCompletingStatement[G] with UnfoldImpl[G]
final case class WandQed[G](res: Expr[G])(implicit val o: Origin) extends NormallyCompletingStatement[G] with WandQedImpl[G]
final case class WandApply[G](res: Expr[G])(implicit val o: Origin) extends NormallyCompletingStatement[G] with WandApplyImpl[G]
final case class WandUse[G](res: Expr[G])(implicit val o: Origin) extends NormallyCompletingStatement[G] with WandUseImpl[G]
final case class Havoc[G](loc: Expr[G])(implicit val o: Origin) extends NormallyCompletingStatement[G] with HavocImpl[G]

sealed trait ExceptionalStatement[G] extends Statement[G] with ExceptionalStatementImpl[G]
final case class Eval[G](expr: Expr[G])(implicit val o: Origin) extends ExceptionalStatement[G] with EvalImpl[G]
sealed trait InvocationStatement[G] extends ExceptionalStatement[G] with InvokingNode[G] with InvocationStatementImpl[G]
final case class InvokeProcedure[G](ref: Ref[G, Procedure[G]], args: Seq[Expr[G]], outArgs: Seq[Ref[G, Variable[G]]], typeArgs: Seq[Type[G]], givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])])(val blame: Blame[InvocationFailure])(implicit val o: Origin) extends InvocationStatement[G] with InvokeProcedureImpl[G]
final case class InvokeMethod[G](obj: Expr[G], ref: Ref[G, InstanceMethod[G]], args: Seq[Expr[G]], outArgs: Seq[Ref[G, Variable[G]]], typeArgs: Seq[Type[G]], givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])])(val blame: Blame[InstanceInvocationFailure])(implicit val o: Origin) extends InvocationStatement[G] with InvokeMethodImpl[G]
final case class Return[G](result: Expr[G])(implicit val o: Origin) extends ExceptionalStatement[G] with ReturnImpl[G]
final case class Throw[G](obj: Expr[G])(val blame: Blame[ThrowNull])(implicit val o: Origin) extends ExceptionalStatement[G] with ThrowImpl[G]
final case class Break[G](label: Option[Ref[G, LabelDecl[G]]])(implicit val o: Origin) extends ExceptionalStatement[G] with BreakImpl[G]
final case class Continue[G](label: Option[Ref[G, LabelDecl[G]]])(implicit val o: Origin) extends ExceptionalStatement[G] with ContinueImpl[G]

sealed trait CompositeStatement[G] extends Statement[G] with CompositeStatementImpl[G]
final case class Block[G](statements: Seq[Statement[G]])(implicit val o: Origin) extends CompositeStatement[G] with BlockImpl[G]
final case class Scope[G](locals: Seq[Variable[G]], body: Statement[G])(implicit val o: Origin) extends CompositeStatement[G] with ScopeImpl[G]
final case class Branch[G](branches: Seq[(Expr[G], Statement[G])])(implicit val o: Origin) extends CompositeStatement[G] with BranchImpl[G]
final case class Switch[G](expr: Expr[G], body: Statement[G])(implicit val o: Origin) extends CompositeStatement[G] with SwitchImpl[G]
final case class Loop[G](init: Statement[G], cond: Expr[G], update: Statement[G], contract: LoopContract[G], body: Statement[G])(implicit val o: Origin) extends CompositeStatement[G] with LoopImpl[G]
final case class TryCatchFinally[G](body: Statement[G], after: Statement[G], catches: Seq[CatchClause[G]])(implicit val o: Origin) extends CompositeStatement[G] with TryCatchFinallyImpl[G]
final case class Synchronized[G](obj: Expr[G], body: Statement[G])(val blame: Blame[UnlockFailure])(implicit val o: Origin) extends CompositeStatement[G] with SynchronizedImpl[G]
final case class ParInvariant[G](decl: ParInvariantDecl[G], inv: Expr[G], content: Statement[G])(val blame: Blame[ParInvariantNotEstablished])(implicit val o: Origin) extends CompositeStatement[G] with ParInvariantImpl[G]
final case class ParAtomic[G](inv: Seq[Ref[G, ParInvariantDecl[G]]], content: Statement[G])(val blame: Blame[ParInvariantNotMaintained])(implicit val o: Origin) extends CompositeStatement[G] with ParAtomicImpl[G]
final case class ParBarrier[G](block: Ref[G, ParBlockDecl[G]], invs: Seq[Ref[G, ParInvariantDecl[G]]], requires: Expr[G], ensures: Expr[G], content: Statement[G])(val blame: Blame[ParBarrierFailed])(implicit val o: Origin) extends CompositeStatement[G] with ParBarrierImpl[G]
final case class ParStatement[G](impl: ParRegion[G])(implicit val o: Origin) extends CompositeStatement[G] with ParStatementImpl[G]
final case class VecBlock[G](iters: Seq[IterVariable[G]], requires: Expr[G], ensures: Expr[G], content: Statement[G])(implicit val o: Origin) extends CompositeStatement[G] with VecBlockImpl[G]
final case class WandCreate[G](statements: Seq[Statement[G]])(implicit val o: Origin) extends CompositeStatement[G] with WandCreateImpl[G]
final case class ModelDo[G](model: Expr[G], perm: Expr[G], after: Expr[G], action: Expr[G], impl: Statement[G])(implicit val o: Origin) extends CompositeStatement[G] with ModelDoImpl[G]

sealed abstract class Declaration[G] extends Node[G] with DeclarationImpl[G] {
  var debugRewriteState: DebugRewriteState = NotProcessed
}

sealed abstract class GlobalDeclaration[G] extends Declaration[G] with GlobalDeclarationImpl[G]
final class SimplificationRule[G](val axiom: Expr[G])(implicit val o: Origin) extends GlobalDeclaration[G] with SimplificationRuleImpl[G]
final class AxiomaticDataType[G](val decls: Seq[ADTDeclaration[G]], val typeArgs: Seq[Variable[G]])(implicit val o: Origin) extends GlobalDeclaration[G] with AxiomaticDataTypeImpl[G]
final class Class[G](val declarations: Seq[ClassDeclaration[G]], val supports: Seq[Ref[G, Class[G]]], val intrinsicLockInvariant: Expr[G], val pin: Option[PinnedDecl[G]] = None)(implicit val o: Origin) extends GlobalDeclaration[G] with ClassImpl[G]
final class Model[G](val declarations: Seq[ModelDeclaration[G]])(implicit val o: Origin) extends GlobalDeclaration[G] with Declarator[G] with ModelImpl[G]
final class Function[G](val returnType: Type[G], val args: Seq[Variable[G]], val typeArgs: Seq[Variable[G]],
               val body: Option[Expr[G]], val contract: ApplicableContract[G], val inline: Boolean = false, val threadLocal: Boolean = false, val pin: Option[PinnedDecl[G]] = None)
              (val blame: Blame[ContractedFailure])(implicit val o: Origin)
  extends GlobalDeclaration[G] with AbstractFunction[G] with FunctionImpl[G]
final class Procedure[G](val returnType: Type[G],
                val args: Seq[Variable[G]], val outArgs: Seq[Variable[G]], val typeArgs: Seq[Variable[G]],
                val body: Option[Statement[G]],
                val contract: ApplicableContract[G],
                val inline: Boolean = false, val pure: Boolean = false)
                        (val blame: Blame[CallableFailure])(implicit val o: Origin)
  extends GlobalDeclaration[G] with AbstractMethod[G] with ProcedureImpl[G]
final class Predicate[G](val args: Seq[Variable[G]], val body: Option[Expr[G]],
                val threadLocal: Boolean = false, val inline: Boolean = false)(implicit val o: Origin)
  extends GlobalDeclaration[G] with AbstractPredicate[G] with PredicateImpl[G]

sealed abstract class ClassDeclaration[G] extends Declaration[G] with ClassDeclarationImpl[G]
final class InstanceFunction[G](val returnType: Type[G], val args: Seq[Variable[G]], val typeArgs: Seq[Variable[G]],
                       val body: Option[Expr[G]], val contract: ApplicableContract[G], val inline: Boolean, val threadLocal: Boolean = false)
                      (val blame: Blame[ContractedFailure])(implicit val o: Origin)
  extends ClassDeclaration[G] with AbstractFunction[G] with InstanceFunctionImpl[G]
final class InstanceMethod[G](val returnType: Type[G],
                     val args: Seq[Variable[G]], val outArgs: Seq[Variable[G]], val typeArgs: Seq[Variable[G]],
                     val body: Option[Statement[G]],
                     val contract: ApplicableContract[G],
                     val inline: Boolean = false, val pure: Boolean = false)
                             (val blame: Blame[CallableFailure])(implicit val o: Origin)
  extends ClassDeclaration[G] with AbstractMethod[G] with InstanceMethodImpl[G]
final class InstancePredicate[G](val args: Seq[Variable[G]], val body: Option[Expr[G]],
                        val threadLocal: Boolean = false, val inline: Boolean = false)(implicit val o: Origin)
  extends ClassDeclaration[G] with AbstractPredicate[G] with InstancePredicateImpl[G]
final class InstanceField[G](val t: Type[G], val flags: Set[FieldFlag[G]])(implicit val o: Origin) extends ClassDeclaration[G] with Field[G] with InstanceFieldImpl[G]

sealed trait ModelDeclaration[G] extends Declaration[G] with ModelDeclarationImpl[G]
final class ModelField[G](val t: Type[G])(implicit val o: Origin) extends ModelDeclaration[G] with Field[G] with ModelFieldImpl[G]
final class ModelProcess[G](val args: Seq[Variable[G]], val impl: Expr[G],
                   val requires: Expr[G], val ensures: Expr[G],
                   val modifies: Seq[Ref[G, ModelField[G]]], val accessible: Seq[Ref[G, ModelField[G]]])
                  (val blame: Blame[PostconditionFailed])
                  (implicit val o: Origin) extends ModelDeclaration[G] with Applicable[G] with ModelProcessImpl[G]
final class ModelAction[G](val args: Seq[Variable[G]],
                  val requires: Expr[G], val ensures: Expr[G],
                  val modifies: Seq[Ref[G, ModelField[G]]], val accessible: Seq[Ref[G, ModelField[G]]])
                 (implicit val o: Origin) extends ModelDeclaration[G] with Applicable[G] with ModelActionImpl[G]

sealed trait ADTDeclaration[G] extends Declaration[G] with ADTDeclarationImpl[G]
final class ADTAxiom[G](val axiom: Expr[G])(implicit val o: Origin) extends ADTDeclaration[G] with ADTAxiomImpl[G]
final class ADTFunction[G](val args: Seq[Variable[G]], val returnType: Type[G])(implicit val o: Origin) extends Applicable[G] with ADTDeclaration[G] with ADTFunctionImpl[G]

final class Variable[G](val t: Type[G])(implicit val o: Origin) extends Declaration[G] with VariableImpl[G]
final class LabelDecl[G]()(implicit val o: Origin) extends Declaration[G] with LabelDeclImpl[G]
final class SendDecl[G]()(implicit val o: Origin) extends Declaration[G] with SendDeclImpl[G]
final class ParBlockDecl[G]()(implicit val o: Origin) extends Declaration[G] with ParBlockDeclImpl[G]
final class ParInvariantDecl[G]()(implicit val o: Origin) extends Declaration[G] with ParInvariantDeclImpl[G]

sealed trait Applicable[G] extends Declaration[G] with ApplicableImpl[G]
sealed trait InlineableApplicable[G] extends Applicable[G] with InlineableApplicableImpl[G]
sealed trait AbstractPredicate[G] extends InlineableApplicable[G] with AbstractPredicateImpl[G]
sealed trait ContractApplicable[G] extends InlineableApplicable[G] with ContractApplicableImpl[G]
sealed trait AbstractFunction[G] extends ContractApplicable[G] with AbstractFunctionImpl[G]
sealed trait AbstractMethod[G] extends ContractApplicable[G] with AbstractMethodImpl[G]
sealed trait Field[G] extends FieldImpl[G]

final case class SignalsClause[G](binding: Variable[G], assn: Expr[G])(implicit val o: Origin) extends NodeFamily[G] with SignalsClauseImpl[G]

final case class ApplicableContract[G](requires: AccountedPredicate[G], ensures: AccountedPredicate[G], contextEverywhere: Expr[G],
                              signals: Seq[SignalsClause[G]], givenArgs: Seq[Variable[G]], yieldsArgs: Seq[Variable[G]])
                             (implicit val o: Origin) extends NodeFamily[G] with ApplicableContractImpl[G]

sealed trait AccountedPredicate[G] extends NodeFamily[G] with AccountedPredicateImpl[G]
case class UnitAccountedPredicate[G](pred: Expr[G])(implicit val o: Origin) extends AccountedPredicate[G] with UnitAccountedPredicateImpl[G]
case class SplitAccountedPredicate[G](left: AccountedPredicate[G], right: AccountedPredicate[G])(implicit val o: Origin) extends AccountedPredicate[G] with SplitAccountedPredicateImpl[G]

sealed trait FieldFlag[G] extends NodeFamily[G] with FieldFlagImpl[G]
final class Final[G]()(implicit val o: Origin) extends FieldFlag[G] with FinalImpl[G]

sealed trait Coercion[G] extends NodeFamily[G] with CoercionImpl[G]
final case class CoerceIdentity[G](source: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceIdentityImpl[G]
final case class CoercionSequence[G](coercions: Seq[Coercion[G]])(implicit val o: Origin) extends Coercion[G] with CoercionSequenceImpl[G]

final case class CoerceNothingSomething[G](target: Type[G])(implicit val o: Origin) extends Coercion[G] with NothingSomethingImpl[G]
final case class CoerceSomethingAny[G](source: Type[G])(implicit val o: Origin) extends Coercion[G] with SomethingAnyImpl[G]

final case class CoerceJoinUnion[G](inner: Seq[Coercion[G]], source: Seq[Type[G]], target: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceJoinUnionImpl[G]
final case class CoerceSelectUnion[G](inner: Coercion[G], source: Type[G], targetAlts: Seq[Type[G]], index: Int)(implicit val o: Origin) extends Coercion[G] with CoerceSelectUnionImpl[G]

final case class CoerceBoolResource[G]()(implicit val o: Origin) extends Coercion[G] with CoerceBoolResourceImpl[G]

final case class CoerceNullRef[G]()(implicit val o: Origin) extends Coercion[G] with CoerceNullRefImpl[G]
final case class CoerceNullArray[G](arrayElementType: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceNullArrayImpl[G]
final case class CoerceNullClass[G](targetClass: Ref[G, Class[G]])(implicit val o: Origin) extends Coercion[G] with CoerceNullClassImpl[G]
final case class CoerceNullJavaClass[G](targetClass: Ref[G, JavaClassOrInterface[G]])(implicit val o: Origin) extends Coercion[G] with CoerceNullJavaClassImpl[G]
final case class CoerceNullPointer[G](pointerElementType: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceNullPointerImpl[G]

final case class CoerceFracZFrac[G]()(implicit val o: Origin) extends Coercion[G] with CoerceFracZFracImpl[G]
final case class CoerceZFracRat[G]()(implicit val o: Origin) extends Coercion[G] with CoerceZFracRatImpl[G]
final case class CoerceFloatRat[G]()(implicit val o: Origin) extends Coercion[G] with CoerceFloatRatImpl[G]
final case class CoerceIntRat[G]()(implicit val o: Origin) extends Coercion[G] with CoerceIntRatImpl[G]

final case class CoerceWidenBound[G](source: Type[G], target: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceWidenBoundImpl[G]
final case class CoerceUnboundInt[G](source: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceUnboundIntImpl[G]

final case class CoerceBoundIntFrac[G]()(implicit val o: Origin) extends Coercion[G] with CoerceBoundIntFracImpl[G]
final case class CoerceBoundIntZFrac[G](source: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceBoundIntZFracImpl[G]

final case class CoerceSupports[G](sourceClass: Ref[G, Class[G]], targetClass: Ref[G, Class[G]])(implicit val o: Origin) extends Coercion[G] with CoerceSupportsImpl[G]
final case class CoerceJavaSupports[G](sourceClass: Ref[G, JavaClassOrInterface[G]], targetClass: Ref[G, JavaClassOrInterface[G]])(implicit val o: Origin) extends Coercion[G] with CoerceJavaSupportsImpl[G]

final case class CoerceCPrimitiveToCol[G](source: Type[G], target: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceCPrimitiveToColImpl[G]
final case class CoerceColToCPrimitive[G](source: Type[G], target: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceColToCPrimitiveImpl[G]

final case class CoerceMapOption[G](inner: Coercion[G], sourceOptionElement: Type[G], targetOptionElement: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceMapOptionImpl[G]
final case class CoerceMapTuple[G](inner: Seq[Coercion[G]], sourceTypes: Seq[Type[G]], targetTypes: Seq[Type[G]])(implicit val o: Origin) extends Coercion[G] with CoerceMapTupleImpl[G]
final case class CoerceMapEither[G](inner: (Coercion[G], Coercion[G]), sourceTypes: (Type[G], Type[G]), targetTypes: (Type[G], Type[G]))(implicit val o: Origin) extends Coercion[G] with CoerceMapEitherImpl[G]
final case class CoerceMapSeq[G](inner: Coercion[G], sourceSeqElement: Type[G], targetSeqElement: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceMapSeqImpl[G]
final case class CoerceMapSet[G](inner: Coercion[G], sourceSetElement: Type[G], targetSetElement: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceMapSetImpl[G]
final case class CoerceMapBag[G](inner: Coercion[G], sourceBagElement: Type[G], targetBagElement: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceMapBagImpl[G]
final case class CoerceMapMatrix[G](inner: Coercion[G], sourceMatrixElement: Type[G], targetMatrixElement: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceMapMatrixImpl[G]
final case class CoerceMapMap[G](inner: Coercion[G], sourceTypes: (Type[G], Type[G]), targetTypes: (Type[G], Type[G]))(implicit val o: Origin) extends Coercion[G] with CoerceMapMapImpl[G]
final case class CoerceMapType[G](inner: Coercion[G], sourceBound: Type[G], targetBound: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceMapTypeImpl[G]

final case class CoerceRatZFrac[G]()(implicit val o: Origin) extends Coercion[G] with CoerceRatZFracImpl[G]
final case class CoerceZFracFrac[G]()(implicit val o: Origin) extends Coercion[G] with CoerceZFracFracImpl[G]

final case class CoerceJavaTClassTPinnedDecl[G](cls: Type[G], pin: PinnedDecl[G])(implicit val o: Origin) extends Coercion[G] with CoerceJavaTClassTPinnedDeclImpl[G]
final case class CoerceTPinnedDeclJavaTClass[G](pin: PinnedDecl[G], cls: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceTPinnedDeclJavaTClassImpl[G]
final case class CoerceTClassTPinnedDecl[G](cls: Type[G], pin: PinnedDecl[G])(implicit val o: Origin) extends Coercion[G] with CoerceTClassTPinnedDeclImpl[G]
final case class CoerceTPinnedDeclTClass[G](pin: PinnedDecl[G], cls: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceTPinnedDeclTClassImpl[G]

sealed trait Expr[G] extends NodeFamily[G] with ExprImpl[G]

sealed abstract class Constant[G, T] extends Expr[G] with ConstantImpl[G, T]
final case class IntegerValue[G](value: BigInt)(implicit val o: Origin) extends Constant[G, BigInt] with Expr[G] with IntegerValueImpl[G]
final case class BooleanValue[G](value: Boolean)(implicit val o: Origin) extends Constant[G, Boolean] with BooleanValueImpl[G]
final case class LiteralSeq[G](element: Type[G], values: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with LiteralSeqImpl[G]
final case class LiteralSet[G](element: Type[G], values: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with LiteralSetImpl[G]
final case class LiteralBag[G](element: Type[G], values: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with LiteralBagImpl[G]
final case class LiteralTuple[G](ts: Seq[Type[G]], values: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with LiteralTupleImpl[G]
final case class LiteralMap[G](k: Type[G], v: Type[G], values: Seq[(Expr[G], Expr[G])])(implicit val o: Origin) extends Expr[G] with LiteralMapImpl[G]
final case class UntypedLiteralSeq[G](values: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with UntypedLiteralSeqImpl[G]
final case class UntypedLiteralSet[G](values: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with UntypedLiteralSetImpl[G]
final case class UntypedLiteralBag[G](values: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with UntypedLiteralBagImpl[G]
final case class Void[G]()(implicit val o: Origin) extends Expr[G] with VoidImpl[G]
final case class Null[G]()(implicit val o: Origin) extends Expr[G] with NullImpl[G]
final case class NoPerm[G]()(implicit val o: Origin) extends Expr[G] with NoPermImpl[G]
final case class WritePerm[G]()(implicit val o: Origin) extends Expr[G] with WritePermImpl[G]
final case class OptSome[G](e: Expr[G])(implicit val o: Origin) extends Expr[G] with OptSomeImpl[G]
final case class OptNone[G]()(implicit val o: Origin) extends Expr[G] with OptNoneImpl[G]
final case class Range[G](from: Expr[G], to: Expr[G])(implicit val o: Origin) extends Expr[G] with RangeImpl[G]
final case class EitherLeft[G](e: Expr[G])(implicit val o: Origin) extends Expr[G] with EitherLeftImpl[G]
final case class EitherRight[G](e: Expr[G])(implicit val o: Origin) extends Expr[G] with EitherRightImpl[G]
final case class MapCons[G](map: Expr[G], k: Expr[G], v: Expr[G])(implicit val o: Origin) extends Expr[G] with MapConsImpl[G]

final case class AmbiguousThis[G]()(implicit val o: Origin) extends Expr[G] with AmbiguousThisImpl[G] {
  var ref: Option[ThisTarget[G]] = None
}

final case class ThisObject[G](cls: Ref[G, Class[G]])(implicit val o: Origin) extends Expr[G] with ThisObjectImpl[G]
final case class ThisModel[G](cls: Ref[G, Model[G]])(implicit val o: Origin) extends Expr[G] with ThisModelImpl[G]

final case class AmbiguousResult[G]()(implicit val o: Origin) extends Expr[G] with AmbiguousResultImpl[G] {
  var ref: Option[ResultTarget[G]] = None
}

final case class Result[G](applicable: Ref[G, ContractApplicable[G]])(implicit val o: Origin) extends Expr[G] with ResultImpl[G]
final case class CurrentThreadId[G]()(implicit val o: Origin) extends Expr[G] with CurrentThreadIdImpl[G]
final case class LocalThreadId[G]()(implicit val o: Origin) extends Expr[G] with LocalThreadIdImpl[G]
final case class GlobalThreadId[G]()(implicit val o: Origin) extends Expr[G] with GlobalThreadIdImpl[G]
final case class Any[G]()(val blame: Blame[AnyStarError])(implicit val o: Origin) extends Expr[G] with AnyImpl[G]
final case class ReadPerm[G]()(implicit val o: Origin) extends Expr[G] with ReadPermImpl[G]
final case class Values[G](arr: Expr[G], from: Expr[G], to: Expr[G])(val blame: Blame[ArrayValuesError])(implicit val o: Origin) extends Expr[G] with ValuesImpl[G]
sealed trait MapCmp[G] extends Expr[G] with MapCmpImpl[G]
final case class MapEq[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends MapCmp[G] with MapEqImpl[G]
final case class MapDisjoint[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends MapCmp[G] with MapDisjointImpl[G]
sealed trait MapOp[G] extends Expr[G] with MapOpImpl[G]
final case class MapKeySet[G](map: Expr[G])(implicit val o: Origin) extends MapOp[G] with MapKeySetImpl[G]
final case class MapValueSet[G](map: Expr[G])(implicit val o: Origin) extends MapOp[G] with MapValueSetImpl[G]
final case class MapItemSet[G](map: Expr[G])(implicit val o: Origin) extends MapOp[G] with MapItemSetImpl[G]
final case class MapRemove[G](map: Expr[G], k: Expr[G])(implicit val o: Origin) extends Expr[G] with MapRemoveImpl[G]

sealed trait Binder[G] extends Expr[G] with BinderImpl[G]
final case class Forall[G](bindings: Seq[Variable[G]], triggers: Seq[Seq[Expr[G]]], body: Expr[G])(implicit val o: Origin) extends Binder[G] with ForallImpl[G]
final case class Starall[G](bindings: Seq[Variable[G]], triggers: Seq[Seq[Expr[G]]], body: Expr[G])(val blame: Blame[ReceiverNotInjective])(implicit val o: Origin) extends Binder[G] with StarallImpl[G]
final case class Exists[G](bindings: Seq[Variable[G]], triggers: Seq[Seq[Expr[G]]], body: Expr[G])(implicit val o: Origin) extends Binder[G] with ExistsImpl[G]
final case class Sum[G](bindings: Seq[Variable[G]], condition: Expr[G], main: Expr[G])(implicit val o: Origin) extends Binder[G] with SumImpl[G]
final case class Product[G](bindings: Seq[Variable[G]], condition: Expr[G], main: Expr[G])(implicit val o: Origin) extends Binder[G] with ProductImpl[G]
final case class Let[G](binding: Variable[G], value: Expr[G], main: Expr[G])(implicit val o: Origin) extends Binder[G] with LetImpl[G]
final case class InlinePattern[G](inner: Expr[G])(implicit val o: Origin) extends Expr[G] with InlinePatternImpl[G]

final case class ScopedExpr[G](declarations: Seq[Variable[G]], body: Expr[G])(implicit val o: Origin) extends Declarator[G] with Expr[G] with ScopedExprImpl[G]

final case class Local[G](ref: Ref[G, Variable[G]])(implicit val o: Origin) extends Expr[G] with LocalImpl[G]
sealed trait HeapDeref[G] extends Expr[G] with HeapDerefImpl[G]
final case class Deref[G](obj: Expr[G], ref: Ref[G, InstanceField[G]])(val blame: Blame[InsufficientPermission])(implicit val o: Origin) extends Expr[G] with HeapDeref[G] with DerefImpl[G]
final case class ModelDeref[G](obj: Expr[G], ref: Ref[G, ModelField[G]])(val blame: Blame[ModelInsufficientPermission])(implicit val o: Origin) extends Expr[G] with ModelDerefImpl[G]
final case class DerefPointer[G](pointer: Expr[G])(val blame: Blame[PointerDerefError])(implicit val o: Origin) extends Expr[G] with DerefPointerImpl[G]
final case class PointerAdd[G](pointer: Expr[G], offset: Expr[G])(val blame: Blame[PointerAddError])(implicit val o: Origin) extends Expr[G] with PointerAddImpl[G]
final case class AddrOf[G](e: Expr[G])(implicit val o: Origin) extends Expr[G] with AddrOfImpl[G]
final case class FunctionOf[G](binding: Ref[G, Variable[G]], vars: Seq[Ref[G, Variable[G]]])(implicit val o: Origin) extends Expr[G] with FunctionOfImpl[G]
final case class ApplyCoercion[G](e: Expr[G], coercion: Coercion[G])(implicit val o: Origin) extends Expr[G] with ApplyCoercionImpl[G]

sealed trait Apply[G] extends Expr[G] with ApplyImpl[G]
final case class ADTFunctionInvocation[G](typeArgs: Option[(Ref[G, AxiomaticDataType[G]], Seq[Type[G]])], ref: Ref[G, ADTFunction[G]], args: Seq[Expr[G]])(implicit val o: Origin) extends Apply[G] with ADTFunctionInvocationImpl[G]

sealed trait ApplyInlineable[G] extends Apply[G] with ApplyInlineableImpl[G]

sealed trait ApplyAnyPredicate[G] extends ApplyInlineable[G] with ApplyAnyPredicateImpl[G]
final case class PredicateApply[G](ref: Ref[G, Predicate[G]], args: Seq[Expr[G]], perm: Expr[G])(implicit val o: Origin) extends ApplyAnyPredicate[G] with PredicateApplyImpl[G]
final case class InstancePredicateApply[G](obj: Expr[G], ref: Ref[G, InstancePredicate[G]], args: Seq[Expr[G]], perm: Expr[G])(implicit val o: Origin) extends ApplyAnyPredicate[G] with InstancePredicateApplyImpl[G]

sealed trait InvokingNode[G] extends Node[G] with InvokingNodeImpl[G]
sealed trait Invocation[G] extends ApplyInlineable[G] with InvokingNode[G] with InvocationImpl[G]

sealed trait AnyMethodInvocation[G] extends Invocation[G] with AnyMethodInvocationImpl[G]
final case class ProcedureInvocation[G](ref: Ref[G, Procedure[G]], args: Seq[Expr[G]], outArgs: Seq[Ref[G, Variable[G]]], typeArgs: Seq[Type[G]], givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])])(val blame: Blame[InvocationFailure])(implicit val o: Origin) extends AnyMethodInvocation[G] with ProcedureInvocationImpl[G]
final case class MethodInvocation[G](obj: Expr[G], ref: Ref[G, InstanceMethod[G]], args: Seq[Expr[G]], outArgs: Seq[Ref[G, Variable[G]]], typeArgs: Seq[Type[G]], givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])])(val blame: Blame[InstanceInvocationFailure])(implicit val o: Origin) extends AnyMethodInvocation[G] with MethodInvocationImpl[G]

sealed trait AnyFunctionInvocation[G] extends Invocation[G] with AnyFunctionInvocationImpl[G]
final case class FunctionInvocation[G](ref: Ref[G, Function[G]], args: Seq[Expr[G]], typeArgs: Seq[Type[G]], givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])])(val blame: Blame[InvocationFailure])(implicit val o: Origin) extends AnyFunctionInvocation[G] with FunctionInvocationImpl[G]
final case class InstanceFunctionInvocation[G](obj: Expr[G], ref: Ref[G, InstanceFunction[G]], args: Seq[Expr[G]], typeArgs: Seq[Type[G]], givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])])(val blame: Blame[InstanceInvocationFailure])(implicit val o: Origin) extends AnyFunctionInvocation[G] with InstanceFunctionInvocationImpl[G]

sealed trait UnExpr[G] extends Expr[G] with UnExprImpl[G]

final case class UMinus[G](arg: Expr[G])(implicit val o: Origin) extends UnExpr[G] with UMinusImpl[G]
final case class BitNot[G](arg: Expr[G])(implicit val o: Origin) extends UnExpr[G] with BitNotImpl[G]
final case class Not[G](arg: Expr[G])(implicit val o: Origin) extends UnExpr[G] with NotImpl[G]

sealed trait BinExpr[G] extends Expr[G] with BinExprImpl[G]
sealed trait NumericBinExpr[G] extends BinExpr[G] with NumericBinExprImpl[G]

sealed trait DividingExpr[G] extends Expr[G] with DividingExprImpl[G]

final case class AmbiguousMult[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends Expr[G] with AmbiguousMultImpl[G]
final case class AmbiguousPlus[G](left: Expr[G], right: Expr[G])(val blame: Blame[FrontendPlusError])(implicit val o: Origin) extends Expr[G] with AmbiguousPlusImpl[G]
final case class AmbiguousMinus[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends Expr[G] with AmbiguousMinusImpl[G]
final case class AmbiguousOr[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with AmbiguousOrImpl[G]

sealed trait BitOp[G] extends BinExpr[G] with BitOpImpl[G]
final case class AmbiguousComputationalOr[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BitOp[G] with AmbiguousComputationalOrImpl[G]
final case class AmbiguousComputationalXor[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BitOp[G] with AmbiguousComputationalXorImpl[G]
final case class AmbiguousComputationalAnd[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BitOp[G] with AmbiguousComputationalAndImpl[G]

final case class ComputationalOr[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with ComputationalOrImpl[G]
final case class ComputationalXor[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with ComputationalXorImpl[G]
final case class ComputationalAnd[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with ComputationalAndImpl[G]

final case class Exp[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends NumericBinExpr[G] with ExpImpl[G]
final case class Plus[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends NumericBinExpr[G] with PlusImpl[G]
final case class Minus[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends NumericBinExpr[G] with MinusImpl[G]
final case class Mult[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends NumericBinExpr[G] with MultImpl[G]
final case class Div[G](left: Expr[G], right: Expr[G])(val blame: Blame[DivByZero])(implicit val o: Origin) extends BinExpr[G] with DividingExpr[G] with DivImpl[G]
final case class FloorDiv[G](left: Expr[G], right: Expr[G])(val blame: Blame[DivByZero])(implicit val o: Origin) extends BinExpr[G] with DividingExpr[G] with FloorDivImpl[G]
final case class Mod[G](left: Expr[G], right: Expr[G])(val blame: Blame[DivByZero])(implicit val o: Origin) extends NumericBinExpr[G] with DividingExpr[G] with ModImpl[G]

final case class StringConcat[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with StringConcatImpl[G]
final case class StringLiteral[G](data: String)(implicit val o: Origin) extends Expr[G] with StringLiteralImpl[G]
final case class JavaStringConcat[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with JavaStringConcatImpl[G]
final case class InternedString[G](data: Expr[G], interner: Ref[G, Function[G]])(implicit val o: Origin) extends Expr[G] with InternedStringImpl[G]

final case class BitAnd[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with BitAndImpl[G]
final case class BitOr[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with BitOrImpl[G]
final case class BitXor[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with BitXorImpl[G]
final case class BitShl[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with BitShlImpl[G]
final case class BitShr[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with BitShrImpl[G]
final case class BitUShr[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with BitUShrImpl[G]

final case class And[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with AndImpl[G]
final case class Or[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with OrImpl[G]
final case class Implies[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with ImpliesImpl[G]
final case class Star[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends Expr[G] with StarImpl[G]
final case class Wand[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends Expr[G] with WandImpl[G]
final case class Scale[G](scale: Expr[G], res: Expr[G])(val blame: Blame[ScaleNegative])(implicit val o: Origin) extends Expr[G] with ScaleImpl[G]

final case class Unfolding[G](res: Expr[G], body: Expr[G])(implicit val o: Origin) extends Expr[G] with UnfoldingImpl[G]

sealed trait Locator[G] extends Expr[G] with LocatorImpl[G]
final case class Perm[G](loc: Expr[G], perm: Expr[G])(implicit val o: Origin) extends Expr[G] with PermImpl[G] with Locator[G]
final case class HPerm[G](loc: Expr[G], perm: Expr[G])(implicit val o: Origin) extends Expr[G] with HPermImpl[G] with Locator[G]
final case class APerm[G](loc: Expr[G], perm: Expr[G])(implicit val o: Origin) extends Expr[G] with APermImpl[G] with Locator[G]
final case class PointsTo[G](loc: Expr[G], perm: Expr[G], value: Expr[G])(implicit val o: Origin) extends Expr[G] with PointsToImpl[G] with Locator[G]

final case class CurPerm[G](loc: Expr[G])(implicit val o: Origin) extends Expr[G] with CurPermImpl[G] with Locator[G]

final case class ValidArray[G](arr: Expr[G], len: Expr[G])(implicit val o: Origin) extends Expr[G] with ValidArrayImpl[G]
final case class ValidMatrix[G](mat: Expr[G], w: Expr[G], h: Expr[G])(implicit val o: Origin) extends Expr[G] with ValidMatrixImpl[G]

final case class PermPointer[G](p: Expr[G], len: Expr[G], perm: Expr[G])(implicit val o: Origin) extends Expr[G] with PermPointerImpl[G]
final case class PermPointerIndex[G](p: Expr[G], idx: Expr[G], perm: Expr[G])(implicit val o: Origin) extends Expr[G] with PermPointerIndexImpl[G]

sealed trait Comparison[G] extends BinExpr[G] with ComparisonImpl[G]
final case class Eq[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends Comparison[G] with EqImpl[G]
final case class Neq[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends Comparison[G] with NeqImpl[G]

sealed trait OrderOp[G] extends Comparison[G] with OrderOpImpl[G]
sealed trait AmbiguousOrderOp[G] extends OrderOp[G] with AmbiguousOrderOpImpl[G]
final case class AmbiguousGreater[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends AmbiguousOrderOp[G] with AmbiguousGreaterImpl[G]
final case class AmbiguousLess[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends AmbiguousOrderOp[G] with AmbiguousLessImpl[G]
final case class AmbiguousGreaterEq[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends AmbiguousOrderOp[G] with AmbiguousGreaterEqImpl[G]
final case class AmbiguousLessEq[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends AmbiguousOrderOp[G] with AmbiguousLessEqImpl[G]

final case class Greater[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends OrderOp[G] with GreaterImpl[G]
final case class Less[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends OrderOp[G] with LessImpl[G]
final case class GreaterEq[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends OrderOp[G] with GreaterEqImpl[G]
final case class LessEq[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends OrderOp[G] with LessEqImpl[G]

sealed trait SetComparison[G] extends OrderOp[G] with SetComparisonImpl[G]
final case class SubSet[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SetComparison[G] with SubSetImpl[G]
final case class SubSetEq[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SetComparison[G] with SubSetEqImpl[G]
final case class SubBag[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SetComparison[G] with SubBagImpl[G]
final case class SubBagEq[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SetComparison[G] with SubBagEqImpl[G]

final case class Select[G](condition: Expr[G], whenTrue: Expr[G], whenFalse: Expr[G])(implicit val o: Origin) extends Expr[G] with SelectImpl[G]
final case class NewObject[G](cls: Ref[G, Class[G]])(implicit val o: Origin) extends Expr[G] with NewObjectImpl[G]
final case class NewArray[G](element: Type[G], dims: Seq[Expr[G]], moreDims: Int)(implicit val o: Origin) extends Expr[G] with NewArrayImpl[G]
final case class Old[G](expr: Expr[G], at: Option[Ref[G, LabelDecl[G]]])(val blame: Blame[LabelNotReached])(implicit val o: Origin) extends Expr[G] with OldImpl[G]
final case class AmbiguousSubscript[G](collection: Expr[G], index: Expr[G])(val blame: Blame[FrontendSubscriptError])(implicit val o: Origin) extends Expr[G] with AmbiguousSubscriptImpl[G]
final case class SeqSubscript[G](seq: Expr[G], index: Expr[G])(val blame: Blame[SeqBoundFailure])(implicit val o: Origin) extends Expr[G] with SeqSubscriptImpl[G]
final case class ArraySubscript[G](arr: Expr[G], index: Expr[G])(val blame: Blame[ArraySubscriptError])(implicit val o: Origin) extends Expr[G] with ArraySubscriptImpl[G]
final case class PointerSubscript[G](pointer: Expr[G], index: Expr[G])(val blame: Blame[PointerSubscriptError])(implicit val o: Origin) extends Expr[G] with PointerSubscriptImpl[G]
final case class Length[G](arr: Expr[G])(val blame: Blame[ArrayNull])(implicit val o: Origin) extends Expr[G] with LengthImpl[G]
final case class Size[G](obj: Expr[G])(implicit val o: Origin) extends Expr[G] with SizeImpl[G]
final case class Cons[G](x: Expr[G], xs: Expr[G])(implicit val o: Origin) extends Expr[G] with ConsImpl[G]

final case class Head[G](xs: Expr[G])(val blame: Blame[SeqBoundFailure])(implicit val o: Origin) extends Expr[G] with HeadImpl[G]
final case class Tail[G](xs: Expr[G])(implicit val o: Origin) extends Expr[G] with TailImpl[G]
final case class Drop[G](xs: Expr[G], count: Expr[G])(implicit val o: Origin) extends Expr[G] with DropImpl[G]
final case class Take[G](xs: Expr[G], count: Expr[G])(implicit val o: Origin) extends Expr[G] with TakeImpl[G]
final case class Slice[G](xs: Expr[G], from: Expr[G], to: Expr[G])(implicit val o: Origin) extends Expr[G] with SliceImpl[G]
final case class SeqUpdate[G](xs: Expr[G], i: Expr[G], x: Expr[G])(implicit val o: Origin) extends Expr[G] with SeqUpdateImpl[G]
final case class Concat[G](xs: Expr[G], ys: Expr[G])(implicit val o: Origin) extends Expr[G] with ConcatImpl[G]
final case class RemoveAt[G](xs: Expr[G], i: Expr[G])(implicit val o: Origin) extends Expr[G] with RemoveAtImpl[G]
final case class Empty[G](obj: Expr[G])(implicit val o: Origin) extends Expr[G] with EmptyImpl[G]

final case class SetIntersection[G](xs: Expr[G], ys: Expr[G])(implicit val o: Origin) extends Expr[G] with SetIntersectionImpl[G]
final case class BagLargestCommon[G](xs: Expr[G], ys: Expr[G])(implicit val o: Origin) extends Expr[G] with BagLargestCommonImpl[G]
final case class SetMinus[G](xs: Expr[G], ys: Expr[G])(implicit val o: Origin) extends Expr[G] with SetMinusImpl[G]
final case class BagMinus[G](xs: Expr[G], ys: Expr[G])(implicit val o: Origin) extends Expr[G] with BagMinusImpl[G]
final case class SetUnion[G](xs: Expr[G], ys: Expr[G])(implicit val o: Origin) extends Expr[G] with SetUnionImpl[G]
final case class BagAdd[G](xs: Expr[G], ys: Expr[G])(implicit val o: Origin) extends Expr[G] with BagAddImpl[G]

final case class AmbiguousMember[G](x: Expr[G], xs: Expr[G])(implicit val o: Origin) extends Expr[G] with AmbiguousMemberImpl[G]
final case class SetMember[G](x: Expr[G], xs: Expr[G])(implicit val o: Origin) extends Expr[G] with SetMemberImpl[G]
final case class SeqMember[G](x: Expr[G], xs: Expr[G])(implicit val o: Origin) extends Expr[G] with SeqMemberImpl[G]
final case class MapMember[G](x: Expr[G], xs: Expr[G])(implicit val o: Origin) extends Expr[G] with MapMemberImpl[G]
final case class BagMemberCount[G](x: Expr[G], xs: Expr[G])(implicit val o: Origin) extends Expr[G] with BagMemberCountImpl[G]

final case class Permutation[G](xs: Expr[G], ys: Expr[G])(implicit val o: Origin) extends Expr[G] with PermutationImpl[G]
final case class OptGet[G](opt: Expr[G])(val blame: Blame[OptionNone])(implicit val o: Origin) extends Expr[G] with OptGetImpl[G]
final case class OptGetOrElse[G](opt: Expr[G], alt: Expr[G])(implicit val o: Origin) extends Expr[G] with OptGetOrElseImpl[G]
final case class MapGet[G](map: Expr[G], k: Expr[G])(val blame: Blame[MapKeyError])(implicit val o: Origin) extends Expr[G] with MapGetImpl[G]
final case class TupGet[G](tup: Expr[G], index: Int)(implicit val o: Origin) extends Expr[G] with TupGetImpl[G]

sealed trait EitherOp[G] extends Expr[G] with EitherOpImpl[G]
final case class GetLeft[G](either: Expr[G])(val blame: Blame[NotLeft])(implicit val o: Origin) extends EitherOp[G] with GetLeftImpl[G]
final case class GetRight[G](either: Expr[G])(val blame: Blame[NotRight])(implicit val o: Origin) extends EitherOp[G] with GetRightImpl[G]
final case class IsLeft[G](either: Expr[G])(implicit val o: Origin) extends EitherOp[G] with Expr[G] with IsLeftImpl[G]
final case class IsRight[G](either: Expr[G])(implicit val o: Origin) extends EitherOp[G] with Expr[G] with IsRightImpl[G]

final case class VectorSum[G](indices: Expr[G], vec: Expr[G])(implicit val o: Origin) extends Expr[G] with VectorSumImpl[G]
final case class VectorCompare[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with VectorCompareImpl[G]
final case class VectorRepeat[G](e: Expr[G])(implicit val o: Origin) extends Expr[G] with VectorRepeatImpl[G]
final case class MatrixSum[G](indices: Expr[G], mat: Expr[G])(implicit val o: Origin) extends Expr[G] with MatrixSumImpl[G]
final case class MatrixCompare[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with MatrixCompareImpl[G]
final case class MatrixRepeat[G](e: Expr[G])(implicit val o: Origin) extends Expr[G] with MatrixRepeatImpl[G]

final case class TypeValue[G](value: Type[G])(implicit val o: Origin) extends Expr[G] with TypeValueImpl[G]
final case class TypeOf[G](expr: Expr[G])(implicit val o: Origin) extends Expr[G] with TypeOfImpl[G]
final case class InstanceOf[G](value: Expr[G], typeValue: Expr[G])(implicit val o: Origin) extends Expr[G] with InstanceOfImpl[G]
final case class Cast[G](value: Expr[G], typeValue: Expr[G])(implicit val o: Origin) extends Expr[G] with CastImpl[G]

sealed trait TypeComparison[G] extends Comparison[G] with TypeComparisonImpl[G]
final case class SubType[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends TypeComparison[G] with SubTypeImpl[G]
final case class SuperType[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends TypeComparison[G] with SuperTypeImpl[G]

sealed trait AssignExpression[G] extends Expr[G] with AssignExpressionImpl[G]
final case class PreAssignExpression[G](target: Expr[G], value: Expr[G])(val blame: Blame[AssignFailed])(implicit val o: Origin) extends AssignExpression[G] with PreAssignExpressionImpl[G]
final case class PostAssignExpression[G](target: Expr[G], value: Expr[G])(val blame: Blame[AssignFailed])(implicit val o: Origin) extends AssignExpression[G] with PostAssignExpressionImpl[G]

final case class With[G](pre: Statement[G], value: Expr[G])(implicit val o: Origin) extends Expr[G] with WithImpl[G]
final case class Then[G](value: Expr[G], post: Statement[G])(implicit val o: Origin) extends Expr[G] with ThenImpl[G]

final case class Held[G](obj: Expr[G])(implicit val o: Origin) extends Expr[G] with HeldImpl[G]
final case class IdleToken[G](thread: Expr[G])(implicit val o: Origin) extends Expr[G] with IdleTokenImpl[G]
final case class JoinToken[G](thread: Expr[G])(implicit val o: Origin) extends Expr[G] with JoinTokenImpl[G]

final case class EmptyProcess[G]()(implicit val o: Origin) extends Expr[G] with EmptyProcessImpl[G]
final case class ActionApply[G](action: Ref[G, ModelAction[G]], args: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with ActionApplyImpl[G]
final case class ProcessApply[G](process: Ref[G, ModelProcess[G]], args: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with ProcessApplyImpl[G]
final case class ProcessSeq[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends Expr[G] with ProcessSeqImpl[G]
final case class ProcessChoice[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends Expr[G] with ProcessChoiceImpl[G]
final case class ProcessPar[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends Expr[G] with ProcessParImpl[G]
final case class ProcessSelect[G](cond: Expr[G], whenTrue: Expr[G], whenFalse: Expr[G])(implicit val o: Origin) extends Expr[G] with ProcessSelectImpl[G]

final case class ModelNew[G](ref: Ref[G, Model[G]])(implicit val o: Origin) extends Expr[G] with ModelNewImpl[G]

final case class ModelState[G](model: Expr[G], perm: Expr[G], state: Expr[G])(implicit val o: Origin) extends Expr[G] with ModelStateImpl[G]
final case class ModelAbstractState[G](model: Expr[G], state: Expr[G])(implicit val o: Origin) extends Expr[G] with ModelAbstractStateImpl[G]
final case class ModelCreate[G](model: Expr[G], init: Expr[G])(implicit val o: Origin) extends Expr[G] with ModelCreateImpl[G]
final case class ModelDestroy[G](model: Expr[G])(implicit val o: Origin) extends Expr[G] with ModelDestroyImpl[G]
final case class ModelSplit[G](model: Expr[G], leftPerm: Expr[G], leftProcess: Expr[G], rightPerm: Expr[G], rightProcess: Expr[G])(implicit val o: Origin) extends Expr[G] with ModelSplitImpl[G]
final case class ModelMerge[G](model: Expr[G], leftPerm: Expr[G], leftProcess: Expr[G], rightPerm: Expr[G], rightProcess: Expr[G])(implicit val o: Origin) extends Expr[G] with ModelMergeImpl[G]
final case class ModelChoose[G](model: Expr[G], perm: Expr[G], totalProcess: Expr[G], choice: Expr[G])(implicit val o: Origin) extends Expr[G] with ModelChooseImpl[G]

final case class ModelPerm[G](loc: Expr[G], perm: Expr[G])(implicit val o: Origin) extends Expr[G] with ModelPermImpl[G]
final case class ActionPerm[G](loc: Expr[G], perm: Expr[G])(implicit val o: Origin) extends Expr[G] with ActionPermImpl[G]

sealed trait CDeclarationSpecifier[G] extends NodeFamily[G] with CDeclarationSpecifierImpl[G]

sealed trait CSpecificationModifier[G] extends CDeclarationSpecifier[G] with CSpecificationModifierImpl[G]
final case class CPure[G]()(implicit val o: Origin) extends CSpecificationModifier[G] with CPureImpl[G]
final case class CInline[G]()(implicit val o: Origin) extends CSpecificationModifier[G] with CInlineImpl[G]

sealed trait CStorageClassSpecifier[G] extends CDeclarationSpecifier[G] with CStorageClassSpecifierImpl[G]
final case class CTypedef[G]()(implicit val o: Origin) extends CStorageClassSpecifier[G] with CTypedefImpl[G]
final case class CExtern[G]()(implicit val o: Origin) extends CStorageClassSpecifier[G] with CExternImpl[G]
final case class CStatic[G]()(implicit val o: Origin) extends CStorageClassSpecifier[G] with CStaticImpl[G]

sealed trait CTypeSpecifier[G] extends CDeclarationSpecifier[G] with CTypeSpecifierImpl[G]
final case class CVoid[G]()(implicit val o: Origin) extends CTypeSpecifier[G] with CVoidImpl[G]
final case class CChar[G]()(implicit val o: Origin) extends CTypeSpecifier[G] with CCharImpl[G]
final case class CShort[G]()(implicit val o: Origin) extends CTypeSpecifier[G] with CShortImpl[G]
final case class CInt[G]()(implicit val o: Origin) extends CTypeSpecifier[G] with CIntImpl[G]
final case class CLong[G]()(implicit val o: Origin) extends CTypeSpecifier[G] with CLongImpl[G]
final case class CFloat[G]()(implicit val o: Origin) extends CTypeSpecifier[G] with CFloatImpl[G]
final case class CDouble[G]()(implicit val o: Origin) extends CTypeSpecifier[G] with CDoubleImpl[G]
final case class CSigned[G]()(implicit val o: Origin) extends CTypeSpecifier[G] with CSignedImpl[G]
final case class CUnsigned[G]()(implicit val o: Origin) extends CTypeSpecifier[G] with CUnsignedImpl[G]
final case class CBool[G]()(implicit val o: Origin) extends CTypeSpecifier[G] with CBoolImpl[G]
final case class CTypedefName[G](name: String)(implicit val o: Origin) extends CTypeSpecifier[G] with CTypedefNameImpl[G] {
  var ref: Option[CTypeNameTarget[G]] = None
}
final case class CSpecificationType[G](t: Type[G])(implicit val o: Origin) extends CTypeSpecifier[G] with CSpecificationTypeImpl[G]

final case class CTypeQualifierDeclarationSpecifier[G](typeQual: CTypeQualifier[G])(implicit val o: Origin) extends CDeclarationSpecifier[G] with CTypeQualifierDeclarationSpecifierImpl[G]

sealed trait CTypeQualifier[G] extends NodeFamily[G] with CTypeQualifierImpl[G]
final case class CConst[G]()(implicit val o: Origin) extends CTypeQualifier[G] with CConstImpl[G]
final case class CRestrict[G]()(implicit val o: Origin) extends CTypeQualifier[G] with CRestrictImpl[G]
final case class CVolatile[G]()(implicit val o: Origin) extends CTypeQualifier[G] with CVolatileImpl[G]
final case class CAtomic[G]()(implicit val o: Origin) extends CTypeQualifier[G] with CAtomicImpl[G]

sealed trait CFunctionSpecifier[G] extends CDeclarationSpecifier[G] with CFunctionSpecifierImpl[G]
sealed trait CAlignmentSpecifier[G] extends CDeclarationSpecifier[G] with CAlignmentSpecifierImpl[G]

sealed trait CGpgpuKernelSpecifier[G] extends CDeclarationSpecifier[G] with CGpgpuKernelSpecifierImpl[G]
final case class CKernel[G]()(implicit val o: Origin) extends CGpgpuKernelSpecifier[G] with CKernelImpl[G]

final case class CPointer[G](qualifiers: Seq[CTypeQualifier[G]])(implicit val o: Origin) extends NodeFamily[G] with CPointerImpl[G]

final class CParam[G](val specifiers: Seq[CDeclarationSpecifier[G]], val declarator: CDeclarator[G])(implicit val o: Origin) extends Declaration[G] with CParamImpl[G]

sealed trait CDeclarator[G] extends NodeFamily[G] with CDeclaratorImpl[G]
final case class CPointerDeclarator[G](pointers: Seq[CPointer[G]], inner: CDeclarator[G])(implicit val o: Origin) extends CDeclarator[G] with CPointerDeclaratorImpl[G]
final case class CArrayDeclarator[G](qualifiers: Seq[CTypeQualifier[G]], size: Option[Expr[G]], inner: CDeclarator[G])(implicit val o: Origin) extends CDeclarator[G] with CArrayDeclaratorImpl[G]
final case class CTypedFunctionDeclarator[G](params: Seq[CParam[G]], varargs: Boolean, inner: CDeclarator[G])(implicit val o: Origin) extends CDeclarator[G] with CTypedFunctionDeclaratorImpl[G]
final case class CAnonymousFunctionDeclarator[G](params: Seq[String], inner: CDeclarator[G])(implicit val o: Origin) extends CDeclarator[G] with CAnonymousFunctionDeclaratorImpl[G]
final case class CName[G](name: String)(implicit val o: Origin) extends CDeclarator[G] with CNameImpl[G]

final case class CInit[G](decl: CDeclarator[G], init: Option[Expr[G]])(implicit val o: Origin) extends NodeFamily[G] with CInitImpl[G]

final class CDeclaration[G](val contract: ApplicableContract[G], val kernelInvariant: Expr[G], val specs: Seq[CDeclarationSpecifier[G]], val inits: Seq[CInit[G]])(implicit val o: Origin) extends Declaration[G] with CDeclarationImpl[G]

sealed trait CAbstractGlobalDeclaration[G] extends GlobalDeclaration[G] with CAbstractGlobalDeclarationImpl[G]

final class CFunctionDefinition[G](val specs: Seq[CDeclarationSpecifier[G]], val declarator: CDeclarator[G], val body: Statement[G])(val blame: Blame[CallableFailure])(implicit val o: Origin) extends CAbstractGlobalDeclaration[G] with CFunctionDefinitionImpl[G]

final class CGlobalDeclaration[G](val decl: CDeclaration[G])(implicit val o: Origin) extends CAbstractGlobalDeclaration[G] with CGlobalDeclarationImpl[G]

sealed trait CStatement[G] extends Statement[G] with CStatementImpl[G]
final case class CDeclarationStatement[G](decl: CDeclaration[G])(implicit val o: Origin) extends CStatement[G] with CDeclarationStatementImpl[G]
final case class CGoto[G](label: String)(implicit val o: Origin) extends CStatement[G] with CGotoImpl[G] {
  var ref: Option[LabelDecl[G]] = None
}

final case class GpgpuLocalBarrier[G](requires: Expr[G], ensures: Expr[G])(implicit val o: Origin) extends CStatement[G] with GpgpuLocalBarrierImpl[G]
final case class GpgpuGlobalBarrier[G](requires: Expr[G], ensures: Expr[G])(implicit val o: Origin) extends CStatement[G] with GpgpuGlobalBarrierImpl[G]
final case class GpgpuAtomic[G](impl: Statement[G], before: Statement[G], after: Statement[G])(implicit val o: Origin) extends CStatement[G] with GpgpuAtomicImpl[G]

sealed trait CExpr[G] extends Expr[G] with CExprImpl[G]
final case class CLocal[G](name: String)(val blame: Blame[DerefInsufficientPermission])(implicit val o: Origin) extends CExpr[G] with CLocalImpl[G] {
  var ref: Option[CNameTarget[G]] = None
}
final case class CInvocation[G](applicable: Expr[G], args: Seq[Expr[G]], givenArgs: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])])(val blame: Blame[FrontendInvocationError])(implicit val o: Origin) extends CExpr[G] with CInvocationImpl[G] {
  var ref: Option[CInvocationTarget[G]] = None
}
final case class CStructAccess[G](struct: Expr[G], field: String)(val blame: Blame[FrontendDerefError])(implicit val o: Origin) extends CExpr[G] with CStructAccessImpl[G] {
  var ref: Option[CDerefTarget[G]] = None
}
final case class CStructDeref[G](struct: Expr[G], field: String)(implicit val o: Origin) extends CExpr[G] with CStructDerefImpl[G]
final case class GpgpuCudaKernelInvocation[G](kernel: String, blocks: Expr[G], threads: Expr[G], args: Seq[Expr[G]], givenArgs: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])])(implicit val o: Origin) extends CExpr[G] with GpgpuCudaKernelInvocationImpl[G] {
  var ref: Option[CInvocationTarget[G]] = None
}

sealed trait CType[G] extends Type[G] with CTypeImpl[G]
final case class CPrimitiveType[G](specifiers: Seq[CDeclarationSpecifier[G]])(implicit val o: Origin = DiagnosticOrigin) extends CType[G] with CPrimitiveTypeImpl[G]

final case class JavaName[G](names: Seq[String])(implicit val o: Origin) extends NodeFamily[G] with JavaNameImpl[G] {
  var ref: Option[JavaTypeNameTarget[G]] = None
}
final case class JavaImport[G](isStatic: Boolean, name: JavaName[G], star: Boolean)(implicit val o: Origin) extends NodeFamily[G] with JavaImportImpl[G]

sealed trait PinnedDecl[G] extends NodeFamily[G] with PinnedDeclImpl[G]
final case class JavaLangString[G]()(implicit val o: Origin = DiagnosticOrigin) extends PinnedDecl[G] with JavaLangStringImpl[G]
final case class JavaStringConcatOperator[G]()(implicit val o: Origin = DiagnosticOrigin) extends PinnedDecl[G] // TODO (RR): Add impl?
final case class JavaLangClass[G]()(implicit val o: Origin = DiagnosticOrigin) extends PinnedDecl[G] // TODO (RR): Add impl?


sealed trait JavaModifier[G] extends NodeFamily[G] with JavaModifierImpl[G]
final case class JavaPublic[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaPublicImpl[G]
final case class JavaProtected[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaProtectedImpl[G]
final case class JavaPrivate[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaPrivateImpl[G]
final case class JavaStatic[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaStaticImpl[G]
final case class JavaAbstract[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaAbstractImpl[G]
final case class JavaFinal[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaFinalImpl[G]
final case class JavaStrictFP[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaStrictFPImpl[G]
final case class JavaNative[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaNativeImpl[G]
final case class JavaSynchronized[G]()(val blame: Blame[UnlockFailure])(implicit val o: Origin) extends JavaModifier[G] with JavaSynchronizedImpl[G]
final case class JavaTransient[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaTransientImpl[G]
final case class JavaVolatile[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaVolatileImpl[G]
final case class JavaAnnotation[G](name: Type[G], args: Seq[(String, Expr[G])])(implicit val o: Origin) extends JavaModifier[G] with JavaAnnotationImpl[G] {
  var data: Option[JavaAnnotationData[G]] = None

  sealed trait JavaAnnotationData[G]
  final case class BIPTransitionData[G](requires: Option[Expr[G]], ensures: Option[Expr[G]])
}

final case class JavaPure[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaPureImpl[G]
final case class JavaInline[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaInlineImpl[G]
final case class JavaBipAnnotation[G]()(implicit val o: Origin) extends JavaModifier[G]

final case class JavaVariableDeclaration[G](name: String, moreDims: Int, init: Option[Expr[G]])(implicit val o: Origin) extends NodeFamily[G] with JavaVariableDeclarationImpl[G]

sealed trait JavaGlobalDeclaration[G] extends GlobalDeclaration[G] with JavaGlobalDeclarationImpl[G]
final class JavaNamespace[G](val pkg: Option[JavaName[G]], val imports: Seq[JavaImport[G]], val declarations: Seq[GlobalDeclaration[G]])(implicit val o: Origin) extends JavaGlobalDeclaration[G] with Declarator[G] with JavaNamespaceImpl[G]

sealed abstract class JavaClassOrInterface[G] extends JavaGlobalDeclaration[G] with Declarator[G] with JavaClassOrInterfaceImpl[G] {
  var pin: Option[PinnedDecl[G]] = None
}
final class JavaClass[G](val name: String, val modifiers: Seq[JavaModifier[G]], val typeParams: Seq[Variable[G]], val intrinsicLockInvariant: Expr[G], val ext: Type[G], val imp: Seq[Type[G]], val decls: Seq[ClassDeclaration[G]])(implicit val o: Origin) extends JavaClassOrInterface[G] with JavaClassImpl[G]
final class JavaInterface[G](val name: String, val modifiers: Seq[JavaModifier[G]], val typeParams: Seq[Variable[G]], val ext: Seq[Type[G]], val decls: Seq[ClassDeclaration[G]])(implicit val o: Origin) extends JavaClassOrInterface[G] with JavaInterfaceImpl[G]
final class JavaAnnotationInterface[G](val name: String, val modifiers: Seq[JavaModifier[G]], val ext: Type[G], val decls: Seq[ClassDeclaration[G]])(implicit val o: Origin) extends JavaClassOrInterface[G] with JavaAnnotationInterfaceImpl[G]

sealed trait JavaClassDeclaration[G] extends ClassDeclaration[G] with JavaClassDeclarationImpl[G]
final class JavaSharedInitialization[G](val isStatic: Boolean, val initialization: Statement[G])(implicit val o: Origin) extends JavaClassDeclaration[G] with JavaSharedInitializationImpl[G]
final class JavaFields[G](val modifiers: Seq[JavaModifier[G]], val t: Type[G], val decls: Seq[JavaVariableDeclaration[G]])(implicit val o: Origin) extends JavaClassDeclaration[G] with JavaFieldsImpl[G]
final class JavaConstructor[G](val modifiers: Seq[JavaModifier[G]], val name: String, val parameters: Seq[Variable[G]], val typeParameters: Seq[Variable[G]], val signals: Seq[Type[G]], val body: Statement[G], val contract: ApplicableContract[G])(val blame: Blame[ConstructorFailure])(implicit val o: Origin) extends JavaClassDeclaration[G] with JavaConstructorImpl[G]
final class JavaMethod[G](val modifiers: Seq[JavaModifier[G]], val returnType: Type[G], val dims: Int, val name: String, val parameters: Seq[Variable[G]], val typeParameters: Seq[Variable[G]], val signals: Seq[Type[G]], val body: Option[Statement[G]], val contract: ApplicableContract[G])(val blame: Blame[CallableFailure])(implicit val o: Origin) extends JavaClassDeclaration[G] with JavaMethodImpl[G]
final class JavaAnnotationMethod[G](val returnType: Type[G], val name: String, val default: Option[Expr[G]])(implicit val o: Origin) extends JavaClassDeclaration[G] with JavaAnnotationMethodImpl[G]

final class JavaLocalDeclaration[G](val modifiers: Seq[JavaModifier[G]], val t: Type[G], val decls: Seq[JavaVariableDeclaration[G]])(implicit val o: Origin) extends Declaration[G] with JavaLocalDeclarationImpl[G]

sealed trait JavaStatement[G] extends Statement[G] with JavaStatementImpl[G]
final case class JavaLocalDeclarationStatement[G](decl: JavaLocalDeclaration[G])(implicit val o: Origin) extends JavaStatement[G] with JavaLocalDeclarationStatementImpl[G]

sealed trait JavaType[G] extends Type[G] with JavaTypeImpl[G]
final case class JavaNamedType[G](names: Seq[(String, Option[Seq[Type[G]]])])(implicit val o: Origin) extends JavaType[G] with JavaNamedTypeImpl[G] {
  var ref: Option[JavaTypeNameTarget[G]] = None
}
final case class JavaTClass[G](ref: Ref[G, JavaClassOrInterface[G]], typeArgs: Seq[Type[G]])(implicit val o: Origin = DiagnosticOrigin) extends JavaType[G] with JavaTClassImpl[G]
final case class TPinnedDecl[G](pin: PinnedDecl[G], typeArgs: Seq[Type[G]])(implicit val o: Origin = DiagnosticOrigin) extends JavaType[G] with TPinnedDeclImpl[G]
final case class Wildcard[G]()(implicit val o: Origin = DiagnosticOrigin) extends JavaType[G] /* with WildcardImpl[G] */

sealed trait JavaExpr[G] extends Expr[G] with JavaExprImpl[G]
final case class JavaLocal[G](name: String)(val blame: Blame[DerefInsufficientPermission])(implicit val o: Origin) extends JavaExpr[G] with JavaLocalImpl[G] {
  var ref: Option[JavaNameTarget[G]] = None
}
final case class JavaDeref[G](obj: Expr[G], field: String)(val blame: Blame[FrontendDerefError])(implicit val o: Origin) extends JavaExpr[G] with JavaDerefImpl[G] {
  var ref: Option[JavaDerefTarget[G]] = None
}
final case class JavaLiteralArray[G](exprs: Seq[Expr[G]])(implicit val o: Origin) extends JavaExpr[G] with JavaLiteralArrayImpl[G] {
  var typeContext: Option[TArray[G]] = None
}
final case class JavaInvocation[G](obj: Option[Expr[G]], typeParams: Seq[Type[G]], method: String, arguments: Seq[Expr[G]], givenArgs: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])])(val blame: Blame[FrontendInvocationError])(implicit val o: Origin) extends JavaExpr[G] with JavaInvocationImpl[G] {
  var ref: Option[JavaInvocationTarget[G]] = None
}
final case class JavaNewClass[G](args: Seq[Expr[G]], typeArgs: Seq[Type[G]], name: Type[G], givenArgs: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])])(val blame: Blame[InvocationFailure])(implicit val o: Origin) extends JavaExpr[G] with JavaNewClassImpl[G] {
  var ref: Option[JavaConstructorTarget[G]] = None
}
final case class JavaNewLiteralArray[G](baseType: Type[G], dims: Int, initializer: Expr[G])(implicit val o: Origin) extends JavaExpr[G] with JavaNewLiteralArrayImpl[G]
final case class JavaNewDefaultArray[G](baseType: Type[G], specifiedDims: Seq[Expr[G]], moreDims: Int)(implicit val o: Origin) extends JavaExpr[G] with JavaNewDefaultArrayImpl[G]
final case class JavaStringLiteral[G](data: String)(implicit val o: Origin) extends JavaExpr[G] with JavaStringLiteralImpl[G]

final case class JavaClassLiteral[G](cls: Type[G])(implicit val o: Origin) extends JavaExpr[G] with JavaClassLiteralImpl[G]

sealed trait PVLType[G] extends Type[G] with PVLTypeImpl[G]
final case class PVLNamedType[G](name: String, typeArgs: Seq[Type[G]])(implicit val o: Origin = DiagnosticOrigin) extends PVLType[G] with PVLNamedTypeImpl[G] {
  var ref: Option[PVLTypeNameTarget[G]] = None
}

sealed trait PVLExpr[G] extends Expr[G] with PVLExprImpl[G]
final case class PVLLocal[G](name: String)(val blame: Blame[DerefInsufficientPermission])(implicit val o: Origin) extends PVLExpr[G] with PVLLocalImpl[G] {
  var ref: Option[PVLNameTarget[G]] = None
}
final case class PVLDeref[G](obj: Expr[G], field: String)(val blame: Blame[FrontendDerefError])(implicit val o: Origin) extends PVLExpr[G] with PVLDerefImpl[G] {
  var ref: Option[PVLDerefTarget[G]] = None
}
final case class PVLInvocation[G](obj: Option[Expr[G]], method: String, args: Seq[Expr[G]], typeArgs: Seq[Type[G]], givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])])(val blame: Blame[FrontendInvocationError])(implicit val o: Origin) extends PVLExpr[G] with PVLInvocationImpl[G] {
  var ref: Option[PVLInvocationTarget[G]] = None
}

final case class PVLNew[G](t: Type[G], args: Seq[Expr[G]], givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])])(val blame: Blame[InvocationFailure])(implicit val o: Origin) extends PVLExpr[G] with PVLNewImpl[G] {
  var ref: Option[PVLConstructorTarget[G]] = None
}

sealed trait PVLClassDeclaration[G] extends ClassDeclaration[G] with PVLClassDeclarationImpl[G]
final class PVLConstructor[G](val contract: ApplicableContract[G], val args: Seq[Variable[G]], val body: Option[Statement[G]])(val blame: Blame[ConstructorFailure])(implicit val o: Origin) extends PVLClassDeclaration[G] with PVLConstructorImpl[G]

sealed trait SilverExpr[G] extends Expr[G] with SilverExprImpl[G]
final case class SilverDeref[G](obj: Expr[G], field: Ref[G, SilverField[G]])(val blame: Blame[InsufficientPermission])(implicit val o: Origin) extends SilverExpr[G] with HeapDeref[G] with SilverDerefImpl[G]
final case class SilverIntToRat[G](perm: Expr[G])(implicit val o: Origin) extends SilverExpr[G] with SilverIntToRatImpl[G]
final case class SilverNull[G]()(implicit val o: Origin) extends SilverExpr[G] with SilverNullImpl[G]
final case class SilverSeqSize[G](seq: Expr[G])(implicit val o: Origin) extends SilverExpr[G] with SilverSeqSizeImpl[G]
final case class SilverSetSize[G](set: Expr[G])(implicit val o: Origin) extends SilverExpr[G] with SilverSetSizeImpl[G]
final case class SilverBagSize[G](bag: Expr[G])(implicit val o: Origin) extends SilverExpr[G] with SilverBagSizeImpl[G]

final case class SilverCurFieldPerm[G](obj: Expr[G], field: Ref[G, SilverField[G]])(implicit val o: Origin) extends SilverExpr[G] with SilverCurFieldPermImpl[G]
final case class SilverCurPredPerm[G](ref: Ref[G, Predicate[G]], args: Seq[Expr[G]])(implicit val o: Origin) extends SilverExpr[G] with SilverCurPredPermImpl[G]

final case class SilverPartialADTFunctionInvocation[G](name: String, args: Seq[Expr[G]], partialTypeArgs: Seq[(Ref[G, Variable[G]], Type[G])])(implicit val o: Origin) extends SilverExpr[G] with SilverPartialADTFunctionInvocationImpl[G] {
  var ref: Option[(AxiomaticDataType[G], ADTFunction[G])] = None
}
final case class SilverUntypedNonemptyLiteralMap[G](values: Seq[(Expr[G], Expr[G])])(implicit val o: Origin) extends SilverExpr[G] with SilverUntypedNonemptyLiteralMapImpl[G]

sealed trait SilverStatement[G] extends Statement[G] with SilverStatementImpl[G]
final case class SilverNewRef[G](v: Ref[G, Variable[G]], fields: Seq[Ref[G, SilverField[G]]])(implicit val o: Origin) extends SilverStatement[G] with SilverNewRefImpl[G]

sealed trait SilverAssign[G] extends SilverStatement[G] with SilverAssignImpl[G]
final case class SilverFieldAssign[G](obj: Expr[G], field: Ref[G, SilverField[G]], value: Expr[G])(val blame: Blame[AssignFailed])(implicit val o: Origin) extends SilverAssign[G] with SilverFieldAssignImpl[G]
final case class SilverLocalAssign[G](v: Ref[G, Variable[G]], value: Expr[G])(implicit val o: Origin) extends SilverAssign[G] with SilverLocalAssignImpl[G]

sealed abstract class SilverDeclaration[G] extends GlobalDeclaration[G] with SilverDeclarationImpl[G]
final class SilverField[G](val t: Type[G])(implicit val o: Origin) extends SilverDeclaration[G] with SilverFieldImpl[G]

sealed trait SilverType[G] extends Type[G] with SilverTypeImpl[G]
case class SilverPartialTAxiomatic[G](ref: Ref[G, AxiomaticDataType[G]], partialTypeArgs: Seq[(Ref[G, Variable[G]], Type[G])])(implicit val o: Origin = DiagnosticOrigin) extends SilverType[G] with SilverPartialTAxiomaticImpl[G]