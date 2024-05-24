package vct.col.ast

import hre.data.BitString
import vct.col.ast.`type`._
import vct.col.ast.`type`.typeclass._
import vct.col.ast.declaration._
import vct.col.ast.declaration.adt._
import vct.col.ast.declaration.category._
import vct.col.ast.declaration.cls._
import vct.col.ast.declaration.global._
import vct.col.ast.declaration.model._
import vct.col.ast.declaration.singular._
import vct.col.ast.expr._
import vct.col.ast.expr.`type`._
import vct.col.ast.expr.ambiguous._
import vct.col.ast.expr.apply._
import vct.col.ast.expr.binder._
import vct.col.ast.expr.bip._
import vct.col.ast.expr.context._
import vct.col.ast.expr.heap._
import vct.col.ast.expr.heap.alloc._
import vct.col.ast.expr.heap.read._
import vct.col.ast.expr.literal.build._
import vct.col.ast.expr.literal.constant._
import vct.col.ast.expr.lock._
import vct.col.ast.expr.misc._
import vct.col.ast.expr.model._
import vct.col.ast.expr.op._
import vct.col.ast.expr.op.bit._
import vct.col.ast.expr.op.bool._
import vct.col.ast.expr.op.cmp._
import vct.col.ast.expr.op.collection._
import vct.col.ast.expr.op.either._
import vct.col.ast.expr.op.map._
import vct.col.ast.expr.op.num._
import vct.col.ast.expr.op.option._
import vct.col.ast.expr.op.process._
import vct.col.ast.expr.op.tuple._
import vct.col.ast.expr.op.vec._
import vct.col.ast.expr.resource._
import vct.col.ast.expr.sideeffect._
import vct.col.ast.family.accountedpredicate._
import vct.col.ast.family.bipdata._
import vct.col.ast.family.bipglueelement._
import vct.col.ast.family.bipporttype._
import vct.col.ast.family.catchclause._
import vct.col.ast.family.coercion._
import vct.col.ast.family.contract._
import vct.col.ast.family.data._
import vct.col.ast.family.decreases._
import vct.col.ast.family.fieldflag._
import vct.col.ast.family.invoking._
import vct.col.ast.family.itervariable._
import vct.col.ast.family.javavar.JavaVariableDeclarationImpl
import vct.col.ast.family.location._
import vct.col.ast.family.loopcontract._
import vct.col.ast.family.parregion._
import vct.col.ast.family.pvlcommunicate._
import vct.col.ast.family.seqguard._
import vct.col.ast.family.seqrun._
import vct.col.ast.family.signals._
import vct.col.ast.lang.c._
import vct.col.ast.lang.cpp._
import vct.col.ast.lang.gpgpu._
import vct.col.ast.lang.java._
import vct.col.ast.lang.llvm._
import vct.col.ast.lang.pvl._
import vct.col.ast.lang.silver._
import vct.col.ast.lang.smt._
import vct.col.ast.lang.sycl._
import vct.col.ast.node._
import vct.col.ast.statement._
import vct.col.ast.statement.behavior.ExpressionContainerStatementImpl
import vct.col.ast.statement.composite._
import vct.col.ast.statement.exceptional._
import vct.col.ast.statement.nonexecutable._
import vct.col.ast.statement.terminal._
import vct.col.ast.statement.veymont._
import vct.col.ast.unsorted._
import vct.col.ast.util.Declarator
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.resolve.ctx._
import vct.col.resolve.lang.JavaAnnotationData
import vct.col.structure.{family, scopes}

/** @inheritdoc */ sealed trait Node[G] extends NodeImpl[G]

sealed trait NodeFamily[G] extends Node[G] with NodeFamilyImpl[G]
sealed trait Declaration[G] extends Node[G] with DeclarationImpl[G]

@family final case class Verification[G](tasks: Seq[VerificationContext[G]], expectedErrors: Seq[ExpectedError])(implicit val o: Origin) extends NodeFamily[G] with VerificationImpl[G]
@family final case class VerificationContext[G](program: Program[G])(implicit val o: Origin) extends NodeFamily[G] with VerificationContextImpl[G]

@family
@scopes[GlobalDeclaration]
@scopes[ClassDeclaration]
@scopes[ADTDeclaration]
@scopes[ModelDeclaration]
@scopes[EnumConstant]
@scopes[Variable]
final case class Program[G](declarations: Seq[GlobalDeclaration[G]])(val blame: Blame[UnsafeCoercion])(implicit val o: Origin) extends NodeFamily[G] with ProgramImpl[G]

@family sealed trait Type[G] extends NodeFamily[G] with TypeImpl[G]

object TNotAValue {
  def unapply[G](t: TNotAValue[G]): Some[Referrable[G]] = Some(t.decl.get)
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
final case class TVector[G](size: BigInt, element: Type[G])(implicit val o: Origin = DiagnosticOrigin) extends CompositeType[G] with TVectorImpl[G]

sealed trait PrimitiveType[G] extends Type[G] with PrimitiveTypeImpl[G]
final case class TAny[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TAnyImpl[G]
final case class TNothing[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TNothingImpl[G]
final case class TAnyValue[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TAnyValueImpl[G]
final case class TVoid[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TVoidImpl[G]
final case class TNull[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TNullImpl[G]
final case class TBool[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TBoolImpl[G]
final case class TResource[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TResourceImpl[G]
final case class TResourceVal[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TResourceValImpl[G]
final case class TChar[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TCharImpl[G]
final case class TString[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TStringImpl[G]
final case class TRef[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TRefImpl[G]
final case class TProcess[G]()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType[G] with TProcessImpl[G]

sealed trait NumericType[G] extends PrimitiveType[G] with NumericTypeImpl[G]
sealed trait FloatType[G] extends NumericType[G] with FloatTypeImpl[G]
sealed trait IntType[G] extends NumericType[G] with IntTypeImpl[G]

final case class TInt[G]()(implicit val o: Origin = DiagnosticOrigin) extends IntType[G] with TIntImpl[G]
final case class TBoundedInt[G](gte: BigInt, lt: BigInt)(implicit val o: Origin = DiagnosticOrigin) extends IntType[G] with TBoundedIntImpl[G]
final case class TFloat[G](exponent: Int, mantissa: Int)(implicit val o: Origin = DiagnosticOrigin) extends FloatType[G] with TFloatImpl[G]
final case class TRational[G]()(implicit val o: Origin = DiagnosticOrigin) extends NumericType[G] with TRationalImpl[G]
final case class TFraction[G]()(implicit val o: Origin = DiagnosticOrigin) extends NumericType[G] with TFractionImpl[G]
final case class TZFraction[G]()(implicit val o: Origin = DiagnosticOrigin) extends NumericType[G] with TZFractionImpl[G]

sealed trait DeclaredType[G] extends Type[G] with DeclaredTypeImpl[G]
final case class TModel[G](model: Ref[G, Model[G]])(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType[G] with TModelImpl[G]
final case class TClass[G](cls: Ref[G, Class[G]], typeArgs: Seq[Type[G]])(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType[G] with TClassImpl[G]
final case class TAnyClass[G]()(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType[G] with TAnyClassImpl[G]
final case class TAxiomatic[G](adt: Ref[G, AxiomaticDataType[G]], args: Seq[Type[G]])(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType[G] with TAxiomaticImpl[G]
final case class TEnum[G](enum: Ref[G, Enum[G]])(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType[G] with TEnumImpl[G]
final case class TProverType[G](ref: Ref[G, ProverType[G]])(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType[G] with TProverTypeImpl[G]
final case class TVeyMontChannel[G](channelType: String)(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType[G] with TVeyMontChannelImpl[G]

@family sealed trait ParRegion[G] extends NodeFamily[G] with ParRegionImpl[G]
final case class ParParallel[G](regions: Seq[ParRegion[G]])(val blame: Blame[ParPreconditionFailed])(implicit val o: Origin) extends ParRegion[G] with ParParallelImpl[G]
final case class ParSequential[G](regions: Seq[ParRegion[G]])(val blame: Blame[ParPreconditionFailed])(implicit val o: Origin) extends ParRegion[G] with ParSequentialImpl[G]
@scopes[Variable] @scopes[SendDecl] @scopes[ParBlockDecl] final case class ParBlock[G](decl: ParBlockDecl[G], iters: Seq[IterVariable[G]], context_everywhere: Expr[G], requires: Expr[G], ensures: Expr[G], content: Statement[G])(val blame: Blame[ParBlockFailure])(implicit val o: Origin) extends ParRegion[G] with ParBlockImpl[G]

@family sealed trait LoopContract[G] extends NodeFamily[G] with LoopContractImpl[G]
final case class LoopInvariant[G](invariant: Expr[G], decreases: Option[DecreasesClause[G]])(val blame: Blame[LoopInvariantFailure])(implicit val o: Origin) extends LoopContract[G] with LoopInvariantImpl[G]
final case class IterationContract[G](requires: Expr[G], ensures: Expr[G], context_everywhere: Expr[G])(val blame: Blame[ParBlockFailure])(implicit val o: Origin) extends LoopContract[G] with IterationContractImpl[G]

@family @scopes[Variable] final case class CatchClause[G](decl: Variable[G], body: Statement[G])(implicit val o: Origin) extends NodeFamily[G] with CatchClauseImpl[G]

@family final case class IterVariable[G](variable: Variable[G], from: Expr[G], to: Expr[G])(implicit val o: Origin) extends NodeFamily[G] with IterVariableImpl[G]

@family sealed trait Statement[G] extends NodeFamily[G] with StatementImpl[G]
sealed trait PurelySequentialStatement[G] extends Statement[G]
sealed trait ControlContainerStatement[G] extends Statement[G]
sealed trait ExpressionContainerStatement[G] extends Statement[G] with ExpressionContainerStatementImpl[G]

final case class PVLBranch[G](branches: Seq[(Expr[G], Statement[G])])(val blame: Blame[FrontendIfFailure])(implicit val o: Origin) extends Statement[G] with ControlContainerStatement[G] with PVLBranchImpl[G]
final case class PVLLoop[G](init: Statement[G], cond: Expr[G], update: Statement[G], contract: LoopContract[G], body: Statement[G])(val blame: Blame[FrontEndLoopFailure])(implicit val o: Origin) extends Statement[G] with ControlContainerStatement[G] with PVLLoopImpl[G]

sealed trait NonExecutableStatement[G] extends Statement[G] with NonExecutableStatementImpl[G]
final case class LocalDecl[G](local: Variable[G])(implicit val o: Origin) extends NonExecutableStatement[G] with PurelySequentialStatement[G] with LocalDeclImpl[G]
final case class SpecIgnoreStart[G]()(implicit val o: Origin) extends NonExecutableStatement[G] with PurelySequentialStatement[G] with SpecIgnoreStartImpl[G]
final case class SpecIgnoreEnd[G]()(implicit val o: Origin) extends NonExecutableStatement[G] with PurelySequentialStatement[G] with SpecIgnoreEndImpl[G]

sealed trait NormallyCompletingStatement[G] extends Statement[G] with NormallyCompletingStatementImpl[G]
final case class Assign[G](target: Expr[G], value: Expr[G])(val blame: Blame[AssignFailed])(implicit val o: Origin) extends NormallyCompletingStatement[G] with AssignImpl[G]
final case class Send[G](decl: SendDecl[G], delta: BigInt, res: Expr[G])(val blame: Blame[SendFailed])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExpressionContainerStatement[G] with SendImpl[G]
final case class Recv[G](ref: Ref[G, SendDecl[G]])(implicit val o: Origin) extends NormallyCompletingStatement[G] with PurelySequentialStatement[G] with RecvImpl[G]
sealed trait SwitchCase[G] extends NormallyCompletingStatement[G] with SwitchCaseImpl[G]
final case class DefaultCase[G]()(implicit val o: Origin) extends SwitchCase[G] with PurelySequentialStatement[G] with DefaultCaseImpl[G]
final case class Case[G](pattern: Expr[G])(implicit val o: Origin) extends SwitchCase[G] with PurelySequentialStatement[G] with CaseImpl[G]
final case class Label[G](decl: LabelDecl[G], stat: Statement[G])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ControlContainerStatement[G] with LabelImpl[G]
final case class Goto[G](lbl: Ref[G, LabelDecl[G]])(implicit val o: Origin) extends NormallyCompletingStatement[G] with GotoImpl[G]
final case class Exhale[G](res: Expr[G])(val blame: Blame[ExhaleFailed])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExpressionContainerStatement[G] with ExhaleImpl[G]
final case class Assert[G](res: Expr[G])(val blame: Blame[AssertFailed])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExpressionContainerStatement[G] with AssertImpl[G]
final case class Refute[G](assn: Expr[G])(val blame: Blame[RefuteFailed])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExpressionContainerStatement[G] with RefuteImpl[G]
final case class Inhale[G](res: Expr[G])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExpressionContainerStatement[G] with InhaleImpl[G]
final case class Assume[G](assn: Expr[G])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExpressionContainerStatement[G] with AssumeImpl[G]
final case class Instantiate[G](cls: Ref[G, Class[G]], out: Expr[G])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExpressionContainerStatement[G] with InstantiateImpl[G]
final case class Wait[G](obj: Expr[G])(val blame: Blame[UnlockFailure])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExpressionContainerStatement[G] with WaitImpl[G]
final case class Notify[G](obj: Expr[G])(val blame: Blame[NotifyFailed])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExpressionContainerStatement[G] with NotifyImpl[G]
final case class Fork[G](obj: Expr[G])(val blame: Blame[ForkFailure])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExpressionContainerStatement[G] with ForkImpl[G]
final case class Join[G](obj: Expr[G])(val blame: Blame[JoinFailure])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExpressionContainerStatement[G] with JoinImpl[G]
final case class Lock[G](obj: Expr[G])(val blame: Blame[LockFailure])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExpressionContainerStatement[G] with LockImpl[G]
final case class Unlock[G](obj: Expr[G])(val blame: Blame[UnlockFailure])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExpressionContainerStatement[G] with UnlockImpl[G]
final case class Commit[G](obj: Expr[G])(val blame: Blame[CommitFailed])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExpressionContainerStatement[G] with CommitImpl[G]
final case class Fold[G](res: Expr[G])(val blame: Blame[FoldFailed])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExpressionContainerStatement[G] with FoldImpl[G]
final case class Unfold[G](res: Expr[G])(val blame: Blame[UnfoldFailed])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExpressionContainerStatement[G] with UnfoldImpl[G]
final case class WandApply[G](res: Expr[G])(val blame: Blame[WandApplyFailed])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExpressionContainerStatement[G] with WandApplyImpl[G]
final case class Havoc[G](loc: Expr[G])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ExpressionContainerStatement[G] with HavocImpl[G]
final case class FramedProof[G](pre: Expr[G], body: Statement[G], post: Expr[G])(val blame: Blame[FramedProofFailure])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ControlContainerStatement[G] with FramedProofImpl[G]
final case class Extract[G](contractedStatement: Statement[G])(implicit val o: Origin) extends NormallyCompletingStatement[G] with ControlContainerStatement[G] with ExtractImpl[G]

sealed trait ExceptionalStatement[G] extends Statement[G] with ExceptionalStatementImpl[G]
final case class Eval[G](expr: Expr[G])(implicit val o: Origin) extends ExceptionalStatement[G] with ControlContainerStatement[G] with EvalImpl[G]
sealed trait InvocationStatement[G] extends ExceptionalStatement[G] with InvokingNode[G] with InvocationStatementImpl[G]
final case class InvokeProcedure[G](ref: Ref[G, Procedure[G]], args: Seq[Expr[G]], outArgs: Seq[Expr[G]], typeArgs: Seq[Type[G]], givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Expr[G], Ref[G, Variable[G]])])(val blame: Blame[InvocationFailure])(implicit val o: Origin) extends InvocationStatement[G] with ControlContainerStatement[G] with InvokeProcedureImpl[G]
final case class InvokeConstructor[G](ref: Ref[G, Constructor[G]], classTypeArgs: Seq[Type[G]], out: Expr[G], args: Seq[Expr[G]], outArgs: Seq[Expr[G]], typeArgs: Seq[Type[G]], givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Expr[G], Ref[G, Variable[G]])])(val blame: Blame[InvocationFailure])(implicit val o: Origin) extends InvocationStatement[G] with ControlContainerStatement[G] with InvokeConstructorImpl[G]
final case class InvokeMethod[G](obj: Expr[G], ref: Ref[G, InstanceMethod[G]], args: Seq[Expr[G]], outArgs: Seq[Expr[G]], typeArgs: Seq[Type[G]], givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Expr[G], Ref[G, Variable[G]])])(val blame: Blame[InstanceInvocationFailure])(implicit val o: Origin) extends InvocationStatement[G] with InstanceApply[G] with ControlContainerStatement[G] with InvokeMethodImpl[G]
final case class Return[G](result: Expr[G])(implicit val o: Origin) extends ExceptionalStatement[G] with ExpressionContainerStatement[G] with ReturnImpl[G]
final case class Throw[G](obj: Expr[G])(val blame: Blame[ThrowNull])(implicit val o: Origin) extends ExceptionalStatement[G] with ExpressionContainerStatement[G] with ThrowImpl[G]
final case class Break[G](label: Option[Ref[G, LabelDecl[G]]])(implicit val o: Origin) extends ExceptionalStatement[G] with BreakImpl[G]
final case class Continue[G](label: Option[Ref[G, LabelDecl[G]]])(implicit val o: Origin) extends ExceptionalStatement[G] with ContinueImpl[G]

sealed trait CompositeStatement[G] extends Statement[G] with CompositeStatementImpl[G]
final case class Block[G](statements: Seq[Statement[G]])(implicit val o: Origin) extends CompositeStatement[G] with ControlContainerStatement[G] with BlockImpl[G]
@scopes[Variable] @scopes[CLocalDeclaration] @scopes[CPPLocalDeclaration] @scopes[JavaLocalDeclaration] final case class Scope[G](locals: Seq[Variable[G]], body: Statement[G])(implicit val o: Origin) extends CompositeStatement[G] with ControlContainerStatement[G] with ScopeImpl[G]
final case class Branch[G](branches: Seq[(Expr[G], Statement[G])])(implicit val o: Origin) extends CompositeStatement[G] with ControlContainerStatement[G] with BranchImpl[G]
final case class IndetBranch[G](branches: Seq[Statement[G]])(implicit val o: Origin) extends CompositeStatement[G] with ControlContainerStatement[G] with IndetBranchImpl[G]
final case class Switch[G](expr: Expr[G], body: Statement[G])(implicit val o: Origin) extends CompositeStatement[G] with ControlContainerStatement[G] with SwitchImpl[G]
@scopes[SendDecl] final case class Loop[G](init: Statement[G], cond: Expr[G], update: Statement[G], contract: LoopContract[G], body: Statement[G])(implicit val o: Origin) extends CompositeStatement[G] with ControlContainerStatement[G] with LoopImpl[G]
@scopes[Variable] final case class RangedFor[G](iter: IterVariable[G], contract: LoopContract[G], body: Statement[G])(implicit val o: Origin) extends CompositeStatement[G] with Declarator[G] with ControlContainerStatement[G] with RangedForImpl[G]
final case class TryCatchFinally[G](body: Statement[G], after: Statement[G], catches: Seq[CatchClause[G]])(implicit val o: Origin) extends CompositeStatement[G] with ControlContainerStatement[G] with TryCatchFinallyImpl[G]
final case class Synchronized[G](obj: Expr[G], body: Statement[G])(val blame: Blame[LockRegionFailure])(implicit val o: Origin) extends CompositeStatement[G] with ControlContainerStatement[G] with SynchronizedImpl[G]
@scopes[ParInvariantDecl] final case class ParInvariant[G](decl: ParInvariantDecl[G], inv: Expr[G], content: Statement[G])(val blame: Blame[ParInvariantNotEstablished])(implicit val o: Origin) extends CompositeStatement[G] with ControlContainerStatement[G] with ParInvariantImpl[G]
final case class ParAtomic[G](inv: Seq[Ref[G, ParInvariantDecl[G]]], content: Statement[G])(val blame: Blame[ParInvariantNotMaintained])(implicit val o: Origin) extends CompositeStatement[G] with ControlContainerStatement[G] with ParAtomicImpl[G]
final case class ParBarrier[G](block: Ref[G, ParBlockDecl[G]], invs: Seq[Ref[G, ParInvariantDecl[G]]], requires: Expr[G], ensures: Expr[G], content: Statement[G])(val blame: Blame[ParBarrierFailure])(implicit val o: Origin) extends CompositeStatement[G] with ControlContainerStatement[G] with ParBarrierImpl[G]
final case class ParStatement[G](impl: ParRegion[G])(implicit val o: Origin) extends CompositeStatement[G] with PurelySequentialStatement[G] with ParStatementImpl[G]
@scopes[Variable] final case class VecBlock[G](iters: Seq[IterVariable[G]], requires: Expr[G], ensures: Expr[G], content: Statement[G])(implicit val o: Origin) extends CompositeStatement[G] with ControlContainerStatement[G] with VecBlockImpl[G]
final case class WandPackage[G](res: Expr[G], proof: Statement[G])(val blame: Blame[PackageFailure])(implicit val o: Origin) extends CompositeStatement[G] with ControlContainerStatement[G] with WandPackageImpl[G]
final case class ModelDo[G](model: Expr[G], perm: Expr[G], after: Expr[G], action: Expr[G], impl: Statement[G])(implicit val o: Origin) extends CompositeStatement[G] with ControlContainerStatement[G] with ModelDoImpl[G]

@family sealed trait GlobalDeclaration[G] extends Declaration[G] with GlobalDeclarationImpl[G]
final class HeapVariable[G](val t: Type[G])(implicit val o: Origin) extends GlobalDeclaration[G] with HeapVariableImpl[G]
final class SimplificationRule[G](val axiom: Expr[G])(implicit val o: Origin) extends GlobalDeclaration[G] with SimplificationRuleImpl[G]
@scopes[Variable] final class AxiomaticDataType[G](val decls: Seq[ADTDeclaration[G]], val typeArgs: Seq[Variable[G]])(implicit val o: Origin) extends GlobalDeclaration[G] with AxiomaticDataTypeImpl[G]
final class Class[G](val typeArgs: Seq[Variable[G]], val decls: Seq[ClassDeclaration[G]], val supports: Seq[Type[G]], val intrinsicLockInvariant: Expr[G])(implicit val o: Origin) extends GlobalDeclaration[G] with ClassImpl[G]
final class Model[G](val declarations: Seq[ModelDeclaration[G]])(implicit val o: Origin) extends GlobalDeclaration[G] with Declarator[G] with ModelImpl[G]
@scopes[LabelDecl] final class Function[G](val returnType: Type[G], val args: Seq[Variable[G]], val typeArgs: Seq[Variable[G]],
               val body: Option[Expr[G]], val contract: ApplicableContract[G], val inline: Boolean = false, val threadLocal: Boolean = false)
              (val blame: Blame[ContractedFailure])(implicit val o: Origin)
  extends GlobalDeclaration[G] with AbstractFunction[G] with FunctionImpl[G]
@scopes[LabelDecl] final class Procedure[G](val returnType: Type[G],
                val args: Seq[Variable[G]], val outArgs: Seq[Variable[G]], val typeArgs: Seq[Variable[G]],
                val body: Option[Statement[G]],
                val contract: ApplicableContract[G],
                val inline: Boolean = false, val pure: Boolean = false, val vesuv_entry: Boolean = false)
                        (val blame: Blame[CallableFailure])(implicit val o: Origin)
  extends GlobalDeclaration[G] with AbstractMethod[G] with ProcedureImpl[G]
final class VeSUVMainMethod[G](val body: Option[Statement[G]])(val blame: Blame[CallableFailure])(implicit val o: Origin) extends GlobalDeclaration[G] with VeSUVMainMethodImpl[G]
@scopes[Variable] final class Predicate[G](val args: Seq[Variable[G]], val body: Option[Expr[G]],
                val threadLocal: Boolean = false, val inline: Boolean = false)(implicit val o: Origin)
  extends GlobalDeclaration[G] with AbstractPredicate[G] with PredicateImpl[G]
final class Enum[G](val constants: Seq[EnumConstant[G]])(implicit val o: Origin) extends GlobalDeclaration[G] with EnumImpl[G]
@family final class EnumConstant[G]()(implicit val o: Origin) extends Declaration[G] with EnumConstantImpl[G]

final class ProverType[G](val interpretation: Seq[(ProverLanguage[G], String)])(implicit val o: Origin) extends GlobalDeclaration[G] with ProverTypeImpl[G]
final class ProverFunction[G](val interpretation: Seq[(ProverLanguage[G], String)], val args: Seq[Variable[G]], val returnType: Type[G])(implicit val o: Origin) extends GlobalDeclaration[G] with Applicable[G] with ProverFunctionImpl[G]

@family sealed trait ProverLanguage[G] extends NodeFamily[G] with ProverLanguageImpl[G]
case class SmtLib[G]()(implicit val o: Origin) extends ProverLanguage[G] with SmtLibImpl[G]
case class Boogie[G]()(implicit val o: Origin) extends ProverLanguage[G] with BoogieImpl[G]

@family sealed trait ClassDeclaration[G] extends Declaration[G] with ClassDeclarationImpl[G]
@scopes[LabelDecl] final class InstanceFunction[G](val returnType: Type[G], val args: Seq[Variable[G]], val typeArgs: Seq[Variable[G]],
                       val body: Option[Expr[G]], val contract: ApplicableContract[G], val inline: Boolean, val threadLocal: Boolean = false)
                      (val blame: Blame[ContractedFailure])(implicit val o: Origin)
  extends ClassDeclaration[G] with AbstractFunction[G] with InstanceFunctionImpl[G]
@scopes[LabelDecl] final class Constructor[G](val cls: Ref[G, Class[G]], val args: Seq[Variable[G]], val outArgs: Seq[Variable[G]], val typeArgs: Seq[Variable[G]], val body: Option[Statement[G]], val contract: ApplicableContract[G], val inline: Boolean = false)(val blame: Blame[CallableFailure])(implicit val o: Origin) extends ClassDeclaration[G] with AbstractMethod[G] with ConstructorImpl[G]
@scopes[LabelDecl] final class InstanceMethod[G](val returnType: Type[G],
                              val args: Seq[Variable[G]], val outArgs: Seq[Variable[G]], val typeArgs: Seq[Variable[G]],
                              val body: Option[Statement[G]],
                              val contract: ApplicableContract[G],
                              val inline: Boolean = false, val pure: Boolean = false)
                             (val blame: Blame[CallableFailure])(implicit val o: Origin)
  extends ClassDeclaration[G] with AbstractMethod[G] with InstanceMethodImpl[G]
@scopes[Variable] final class InstancePredicate[G](val args: Seq[Variable[G]], val body: Option[Expr[G]],
                        val threadLocal: Boolean = false, val inline: Boolean = false)(implicit val o: Origin)
  extends ClassDeclaration[G] with AbstractPredicate[G] with InstancePredicateImpl[G]
final class InstanceField[G](val t: Type[G], val flags: Seq[FieldFlag[G]])(implicit val o: Origin) extends ClassDeclaration[G] with Field[G] with InstanceFieldImpl[G]
final class RunMethod[G](val body: Option[Statement[G]], val contract: ApplicableContract[G])(val blame: Blame[CallableFailure])(implicit val o: Origin) extends ClassDeclaration[G] with RunMethodImpl[G]
final class InstanceOperatorFunction[G](val returnType: Type[G], val operator: Operator[G], val args: Seq[Variable[G]],
                                        val body: Option[Expr[G]], val contract: ApplicableContract[G],
                                        val inline: Boolean, val threadLocal: Boolean = false)
                                       (val blame: Blame[ContractedFailure])(implicit val o: Origin)
  extends ClassDeclaration[G] with AbstractFunction[G] with InstanceOperatorFunctionImpl[G]
final class InstanceOperatorMethod[G](val returnType: Type[G],
                                      val operator: Operator[G],
                                      val args: Seq[Variable[G]],
                                      val body: Option[Statement[G]],
                                      val contract: ApplicableContract[G],
                                      val inline: Boolean = false, val pure: Boolean = false)
                                     (val blame: Blame[CallableFailure])(implicit val o: Origin)
  extends ClassDeclaration[G] with AbstractMethod[G] with InstanceOperatorMethodImpl[G]

@family sealed trait Operator[G] extends NodeFamily[G] with OperatorImpl[G]
case class OperatorLeftPlus[G]()(implicit val o: Origin = DiagnosticOrigin) extends Operator[G] with OperatorLeftPlusImpl[G]
case class OperatorRightPlus[G]()(implicit val o: Origin = DiagnosticOrigin) extends Operator[G] with OperatorRightPlusImpl[G]

@family sealed trait ModelDeclaration[G] extends Declaration[G] with ModelDeclarationImpl[G]
final class ModelField[G](val t: Type[G])(implicit val o: Origin) extends ModelDeclaration[G] with Field[G] with ModelFieldImpl[G]
@scopes[Variable] final class ModelProcess[G](val args: Seq[Variable[G]], val impl: Expr[G],
                   val requires: Expr[G], val ensures: Expr[G],
                   val modifies: Seq[Ref[G, ModelField[G]]], val accessible: Seq[Ref[G, ModelField[G]]])
                  (val blame: Blame[PostconditionFailed])
                  (implicit val o: Origin) extends ModelDeclaration[G] with Applicable[G] with ModelProcessImpl[G]
@scopes[Variable] final class ModelAction[G](val args: Seq[Variable[G]],
                  val requires: Expr[G], val ensures: Expr[G],
                  val modifies: Seq[Ref[G, ModelField[G]]], val accessible: Seq[Ref[G, ModelField[G]]])
                 (implicit val o: Origin) extends ModelDeclaration[G] with Applicable[G] with ModelActionImpl[G]

@family sealed trait ADTDeclaration[G] extends Declaration[G] with ADTDeclarationImpl[G]
final class ADTAxiom[G](val axiom: Expr[G])(implicit val o: Origin) extends ADTDeclaration[G] with ADTAxiomImpl[G]
@scopes[Variable] final class ADTFunction[G](val args: Seq[Variable[G]], val returnType: Type[G])(implicit val o: Origin) extends Applicable[G] with ADTDeclaration[G] with ADTFunctionImpl[G]

@family final class Variable[G](val t: Type[G])(implicit val o: Origin) extends Declaration[G] with VariableImpl[G]
@family final class LabelDecl[G]()(implicit val o: Origin) extends Declaration[G] with LabelDeclImpl[G]
@family final class SendDecl[G]()(implicit val o: Origin) extends Declaration[G] with SendDeclImpl[G]
@family final class ParBlockDecl[G]()(implicit val o: Origin) extends Declaration[G] with ParBlockDeclImpl[G]
@family final class ParInvariantDecl[G]()(implicit val o: Origin) extends Declaration[G] with ParInvariantDeclImpl[G]

sealed trait Applicable[G] extends ApplicableImpl[G] with Declaration[G]
sealed trait InlineableApplicable[G] extends Applicable[G] with InlineableApplicableImpl[G]
sealed trait AbstractPredicate[G] extends InlineableApplicable[G] with AbstractPredicateImpl[G]
sealed trait ContractApplicable[G] extends InlineableApplicable[G] with ContractApplicableImpl[G]
sealed trait AbstractFunction[G] extends ContractApplicable[G] with AbstractFunctionImpl[G]
sealed trait AbstractMethod[G] extends ContractApplicable[G] with AbstractMethodImpl[G]
sealed trait Field[G] extends FieldImpl[G]

@family @scopes[Variable] final case class SignalsClause[G](binding: Variable[G], assn: Expr[G])(implicit val o: Origin) extends NodeFamily[G] with SignalsClauseImpl[G]

@family sealed trait DecreasesClause[G] extends NodeFamily[G] with DecreasesClauseImpl[G]
final case class DecreasesClauseAssume[G]()(implicit val o: Origin) extends DecreasesClause[G] with DecreasesClauseAssumeImpl[G]
final case class DecreasesClauseNoRecursion[G]()(implicit val o: Origin) extends DecreasesClause[G] with DecreasesClauseNoRecursionImpl[G]
final case class DecreasesClauseTuple[G](exprs: Seq[Expr[G]])(implicit val o: Origin) extends DecreasesClause[G] with DecreasesClauseTupleImpl[G]

@family final case class ApplicableContract[G](requires: AccountedPredicate[G], ensures: AccountedPredicate[G], contextEverywhere: Expr[G],
                                       signals: Seq[SignalsClause[G]], givenArgs: Seq[Variable[G]], yieldsArgs: Seq[Variable[G]], decreases: Option[DecreasesClause[G]])
                                      (val blame: Blame[NontrivialUnsatisfiable])(implicit val o: Origin) extends NodeFamily[G] with ApplicableContractImpl[G]

/** @inheritdoc */ @family sealed trait AccountedPredicate[G] extends NodeFamily[G] with AccountedPredicateImpl[G]
case class UnitAccountedPredicate[G](pred: Expr[G])(implicit val o: Origin) extends AccountedPredicate[G] with UnitAccountedPredicateImpl[G]
case class SplitAccountedPredicate[G](left: AccountedPredicate[G], right: AccountedPredicate[G])(implicit val o: Origin) extends AccountedPredicate[G] with SplitAccountedPredicateImpl[G]

@family sealed trait FieldFlag[G] extends NodeFamily[G] with FieldFlagImpl[G]
final case class Final[G]()(implicit val o: Origin) extends FieldFlag[G] with FinalImpl[G]

@family sealed trait Coercion[G] extends NodeFamily[G] with CoercionImpl[G]
final case class CoerceIdentity[G](source: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceIdentityImpl[G]
final case class CoercionSequence[G](coercions: Seq[Coercion[G]])(implicit val o: Origin) extends Coercion[G] with CoercionSequenceImpl[G]

final case class CoerceNothingSomething[G](target: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceNothingSomethingImpl[G]
final case class CoerceSomethingAny[G](source: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceSomethingAnyImpl[G]
final case class CoerceSomethingAnyValue[G](source: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceSomethingAnyValueImpl[G]

final case class CoerceJoinUnion[G](inner: Seq[Coercion[G]], source: Seq[Type[G]], target: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceJoinUnionImpl[G]
final case class CoerceSelectUnion[G](inner: Coercion[G], source: Type[G], targetAlts: Seq[Type[G]], index: Int)(implicit val o: Origin) extends Coercion[G] with CoerceSelectUnionImpl[G]

final case class CoerceBoolResource[G]()(implicit val o: Origin) extends Coercion[G] with CoerceBoolResourceImpl[G]
final case class CoerceResourceResourceVal[G]()(implicit val o: Origin) extends Coercion[G] with CoerceResourceResourceValImpl[G]
final case class CoerceResourceValResource[G]()(implicit val o: Origin) extends Coercion[G] with CoerceResourceValResourceImpl[G]

final case class CoerceNullRef[G]()(implicit val o: Origin) extends Coercion[G] with CoerceNullRefImpl[G]
final case class CoerceNullArray[G](arrayElementType: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceNullArrayImpl[G]
final case class CoerceNullClass[G](targetClass: Ref[G, Class[G]], typeArgs: Seq[Type[G]])(implicit val o: Origin) extends Coercion[G] with CoerceNullClassImpl[G]
final case class CoerceNullJavaClass[G](targetClass: Ref[G, JavaClassOrInterface[G]])(implicit val o: Origin) extends Coercion[G] with CoerceNullJavaClassImpl[G]
final case class CoerceNullAnyClass[G]()(implicit val o: Origin) extends Coercion[G] with CoerceNullAnyClassImpl[G]
final case class CoerceNullPointer[G](pointerElementType: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceNullPointerImpl[G]
final case class CoerceNullEnum[G](targetEnum: Ref[G, Enum[G]])(implicit val o: Origin) extends Coercion[G] with CoerceNullEnumImpl[G]

final case class CoerceCArrayPointer[G](elementType: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceCArrayPointerImpl[G]
final case class CoerceCPPArrayPointer[G](elementType: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceCPPArrayPointerImpl[G]
final case class CoerceCVectorVector[G](size: BigInt, elementType: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceCVectorVectorImpl[G]

final case class CoerceFracZFrac[G]()(implicit val o: Origin) extends Coercion[G] with CoerceFracZFracImpl[G]
final case class CoerceZFracRat[G]()(implicit val o: Origin) extends Coercion[G] with CoerceZFracRatImpl[G]
final case class CoerceFloatRat[G](source: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceFloatRatImpl[G]
final case class CoerceCFloatCInt[G](source: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceCFloatCIntImpl[G]
final case class CoerceCIntCFloat[G](target: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceCIntCFloatImpl[G]
final case class CoerceCIntInt[G]()(implicit val o: Origin) extends Coercion[G] with CoerceCIntIntImpl[G]
final case class CoerceCFloatFloat[G](source: Type[G], target: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceCFloatFloatImpl[G]
final case class CoerceIntRat[G]()(implicit val o: Origin) extends Coercion[G] with CoerceIntRatImpl[G]

final case class CoerceIncreasePrecision[G](source: Type[G], target: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceIncreasePrecisionImpl[G]
final case class CoerceDecreasePrecision[G](source: Type[G], target: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceDecreasePrecisionImpl[G]

final case class CoerceWidenBound[G](source: Type[G], target: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceWidenBoundImpl[G]
final case class CoerceUnboundInt[G](source: Type[G], target: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceUnboundIntImpl[G]

final case class CoerceBoundIntFrac[G]()(implicit val o: Origin) extends Coercion[G] with CoerceBoundIntFracImpl[G]
final case class CoerceBoundIntZFrac[G](source: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceBoundIntZFracImpl[G]
final case class CoerceBoundIntFloat[G](source: Type[G], target: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceBoundIntFloatImpl[G]

final case class CoerceSupports[G](sourceClass: Ref[G, Class[G]], targetClass: Ref[G, Class[G]])(implicit val o: Origin) extends Coercion[G] with CoerceSupportsImpl[G]
final case class CoerceJavaSupports[G](sourceClass: Ref[G, JavaClassOrInterface[G]], targetClass: Ref[G, JavaClassOrInterface[G]])(implicit val o: Origin) extends Coercion[G] with CoerceJavaSupportsImpl[G]
final case class CoerceClassAnyClass[G](sourceClass: Ref[G, Class[G]], typeArgs: Seq[Type[G]])(implicit val o: Origin) extends Coercion[G] with CoerceClassAnyClassImpl[G]
final case class CoerceJavaClassAnyClass[G](sourceClass: Ref[G, JavaClassOrInterface[G]])(implicit val o: Origin) extends Coercion[G] with CoerceJavaClassAnyClassImpl[G]

final case class CoerceCPrimitiveToCol[G](source: Type[G], target: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceCPrimitiveToColImpl[G]
final case class CoerceColToCPrimitive[G](source: Type[G], target: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceColToCPrimitiveImpl[G]

final case class CoerceCPPPrimitiveToCol[G](source: Type[G], target: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceCPPPrimitiveToColImpl[G]
final case class CoerceColToCPPPrimitive[G](source: Type[G], target: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceColToCPPPrimitiveImpl[G]

final case class CoerceMapOption[G](inner: Coercion[G], sourceOptionElement: Type[G], targetOptionElement: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceMapOptionImpl[G]
final case class CoerceMapTuple[G](inner: Seq[Coercion[G]], sourceTypes: Seq[Type[G]], targetTypes: Seq[Type[G]])(implicit val o: Origin) extends Coercion[G] with CoerceMapTupleImpl[G]
final case class CoerceMapEither[G](inner: (Coercion[G], Coercion[G]), sourceTypes: (Type[G], Type[G]), targetTypes: (Type[G], Type[G]))(implicit val o: Origin) extends Coercion[G] with CoerceMapEitherImpl[G]
final case class CoerceMapSeq[G](inner: Coercion[G], sourceSeqElement: Type[G], targetSeqElement: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceMapSeqImpl[G]
final case class CoerceMapSet[G](inner: Coercion[G], sourceSetElement: Type[G], targetSetElement: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceMapSetImpl[G]
final case class CoerceMapVector[G](inner: Coercion[G], sourceVectorElement: Type[G], targetVectorElement: Type[G], size: BigInt)(implicit val o: Origin) extends Coercion[G] with CoerceMapVectorImpl[G]
final case class CoerceMapBag[G](inner: Coercion[G], sourceBagElement: Type[G], targetBagElement: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceMapBagImpl[G]
final case class CoerceMapMatrix[G](inner: Coercion[G], sourceMatrixElement: Type[G], targetMatrixElement: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceMapMatrixImpl[G]
final case class CoerceMapMap[G](inner: Coercion[G], sourceTypes: (Type[G], Type[G]), targetTypes: (Type[G], Type[G]))(implicit val o: Origin) extends Coercion[G] with CoerceMapMapImpl[G]
final case class CoerceMapType[G](inner: Coercion[G], sourceBound: Type[G], targetBound: Type[G])(implicit val o: Origin) extends Coercion[G] with CoerceMapTypeImpl[G]

final case class CoerceRatZFrac[G]()(implicit val o: Origin) extends Coercion[G] with CoerceRatZFracImpl[G]
final case class CoerceZFracFrac[G]()(implicit val o: Origin) extends Coercion[G] with CoerceZFracFracImpl[G]

@family sealed trait Expr[G] extends NodeFamily[G] with ExprImpl[G]

sealed trait Constant[G] extends Expr[G] with ConstantImpl[G]
sealed trait ConstantInt[G] extends Constant[G] with ConstantIntImpl[G]
final case class CIntegerValue[G](value: BigInt)(implicit val o: Origin) extends ConstantInt[G] with Expr[G] with CIntegerValueImpl[G]
final case class IntegerValue[G](value: BigInt)(implicit val o: Origin) extends ConstantInt[G] with Expr[G] with IntegerValueImpl[G]
final case class BooleanValue[G](value: Boolean)(implicit val o: Origin) extends Constant[G] with BooleanValueImpl[G]
final case class FloatValue[G](value: BigDecimal, t: Type[G] /* TFloat */)(implicit val o: Origin) extends Constant[G] with FloatValueImpl[G]
final case class StringValue[G](value: String)(implicit val o: Origin) extends Constant[G] with StringValueImpl[G]
final case class CharValue[G](value: Int)(implicit val o: Origin) extends Constant[G] with CharValueImpl[G]
final case class LiteralSeq[G](element: Type[G], values: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with LiteralSeqImpl[G]
final case class LiteralSet[G](element: Type[G], values: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with LiteralSetImpl[G]
final case class LiteralBag[G](element: Type[G], values: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with LiteralBagImpl[G]
final case class LiteralTuple[G](ts: Seq[Type[G]], values: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with LiteralTupleImpl[G]
final case class LiteralMap[G](k: Type[G], v: Type[G], values: Seq[(Expr[G], Expr[G])])(implicit val o: Origin) extends Expr[G] with LiteralMapImpl[G]
final case class LiteralVector[G](element: Type[G], values: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with LiteralVectorImpl[G]
final case class UntypedLiteralSeq[G](values: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with UntypedLiteralSeqImpl[G]
final case class UntypedLiteralSet[G](values: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with UntypedLiteralSetImpl[G]
final case class UntypedLiteralBag[G](values: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with UntypedLiteralBagImpl[G]
final case class Void[G]()(implicit val o: Origin) extends Expr[G] with VoidImpl[G]
final case class Null[G]()(implicit val o: Origin) extends Expr[G] with NullImpl[G]
final case class NoPerm[G]()(implicit val o: Origin) extends Expr[G] with NoPermImpl[G]
final case class WritePerm[G]()(implicit val o: Origin) extends Expr[G] with WritePermImpl[G]
final case class OptSome[G](e: Expr[G])(implicit val o: Origin) extends Expr[G] with OptSomeImpl[G]
final case class OptSomeTyped[G](element: Type[G], e: Expr[G])(implicit val o: Origin) extends Expr[G] with OptSomeTypedImpl[G]
final case class OptNone[G]()(implicit val o: Origin) extends Expr[G] with OptNoneImpl[G]
final case class OptNoneTyped[G](element: Type[G])(implicit val o: Origin) extends Expr[G] with OptNoneTypedImpl[G]
final case class Range[G](from: Expr[G], to: Expr[G])(implicit val o: Origin) extends Expr[G] with RangeImpl[G]
final case class RangeSet[G](from: Expr[G], to: Expr[G])(implicit val o: Origin) extends Expr[G] with RangeSetImpl[G]
final case class EitherLeft[G](e: Expr[G])(implicit val o: Origin) extends Expr[G] with EitherLeftImpl[G]
final case class EitherRight[G](e: Expr[G])(implicit val o: Origin) extends Expr[G] with EitherRightImpl[G]
final case class MapCons[G](map: Expr[G], k: Expr[G], v: Expr[G])(implicit val o: Origin) extends Expr[G] with MapConsImpl[G]

final case class AmbiguousThis[G]()(implicit val o: Origin) extends Expr[G] with AmbiguousThisImpl[G] {
  var ref: Option[ThisTarget[G]] = None
}

sealed trait ThisDeclaration[G] extends Expr[G] with ThisDeclarationImpl[G]
final case class ThisObject[G](cls: Ref[G, Class[G]])(implicit val o: Origin) extends ThisDeclaration[G] with ThisObjectImpl[G]
final case class ThisModel[G](cls: Ref[G, Model[G]])(implicit val o: Origin) extends ThisDeclaration[G] with ThisModelImpl[G]
final case class ThisChoreography[G](cls: Ref[G, Choreography[G]])(implicit val o: Origin) extends ThisDeclaration[G] with ThisChoreographyImpl[G]

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
@scopes[Variable] final case class Forall[G](bindings: Seq[Variable[G]], triggers: Seq[Seq[Expr[G]]], body: Expr[G])(implicit val o: Origin) extends Binder[G] with ForallImpl[G]
@scopes[Variable] final case class Starall[G](bindings: Seq[Variable[G]], triggers: Seq[Seq[Expr[G]]], body: Expr[G])(val blame: Blame[ReceiverNotInjective])(implicit val o: Origin) extends Binder[G] with StarallImpl[G]
@scopes[Variable] final case class Exists[G](bindings: Seq[Variable[G]], triggers: Seq[Seq[Expr[G]]], body: Expr[G])(implicit val o: Origin) extends Binder[G] with ExistsImpl[G]
@scopes[Variable] final case class Sum[G](bindings: Seq[Variable[G]], condition: Expr[G], main: Expr[G])(implicit val o: Origin) extends Binder[G] with SumImpl[G]
@scopes[Variable] final case class Product[G](bindings: Seq[Variable[G]], condition: Expr[G], main: Expr[G])(implicit val o: Origin) extends Binder[G] with ProductImpl[G]
@scopes[Variable] final case class ForPerm[G](bindings: Seq[Variable[G]], loc: Location[G], body: Expr[G])(implicit val o: Origin) extends Binder[G] with ForPermImpl[G]
@scopes[Variable] final case class ForPermWithValue[G](binding: Variable[G], body: Expr[G])(implicit val o: Origin) extends Binder[G] with ForPermWithValueImpl[G]
@scopes[Variable] final case class Let[G](binding: Variable[G], value: Expr[G], main: Expr[G])(implicit val o: Origin) extends Binder[G] with LetImpl[G]
final case class InlinePattern[G](inner: Expr[G], parent: Int = 0, group: Int = 0)(implicit val o: Origin) extends Expr[G] with InlinePatternImpl[G]

@scopes[Variable] final case class ScopedExpr[G](declarations: Seq[Variable[G]], body: Expr[G])(implicit val o: Origin) extends Declarator[G] with Expr[G] with ScopedExprImpl[G]

final case class Local[G](ref: Ref[G, Variable[G]])(implicit val o: Origin) extends Expr[G] with LocalImpl[G]
final case class EnumUse[G](enum: Ref[G, Enum[G]], const: Ref[G, EnumConstant[G]])(implicit val o: Origin) extends Expr[G] with EnumUseImpl[G]
sealed trait HeapDeref[G] extends Expr[G] with HeapDerefImpl[G]
final case class DerefHeapVariable[G](ref: Ref[G, HeapVariable[G]])(val blame: Blame[InsufficientPermission])(implicit val o: Origin) extends Expr[G] with HeapDeref[G] with DerefHeapVariableImpl[G]
final case class Deref[G](obj: Expr[G], ref: Ref[G, InstanceField[G]])(val blame: Blame[InsufficientPermission])(implicit val o: Origin) extends Expr[G] with HeapDeref[G] with DerefImpl[G]
final case class ModelDeref[G](obj: Expr[G], ref: Ref[G, ModelField[G]])(val blame: Blame[ModelInsufficientPermission])(implicit val o: Origin) extends Expr[G] with ModelDerefImpl[G]
final case class DerefPointer[G](pointer: Expr[G])(val blame: Blame[PointerDerefError])(implicit val o: Origin) extends Expr[G] with DerefPointerImpl[G]
final case class PointerAdd[G](pointer: Expr[G], offset: Expr[G])(val blame: Blame[PointerAddError])(implicit val o: Origin) extends Expr[G] with PointerAddImpl[G]
final case class AddrOf[G](e: Expr[G])(implicit val o: Origin) extends Expr[G] with AddrOfImpl[G]
final case class FunctionOf[G](binding: Ref[G, Variable[G]], vars: Seq[Ref[G, Variable[G]]])(implicit val o: Origin) extends Expr[G] with FunctionOfImpl[G]
final case class ApplyCoercion[G](e: Expr[G], coercion: Coercion[G])(implicit val o: Origin) extends Expr[G] with ApplyCoercionImpl[G]
final case class SizeOf[G](tname: Type[G])(implicit val o: Origin) extends Expr[G] with SizeOfImpl[G]

sealed trait Apply[G] extends Expr[G] with ApplyImpl[G]
final case class ADTFunctionInvocation[G](typeArgs: Option[(Ref[G, AxiomaticDataType[G]], Seq[Type[G]])], ref: Ref[G, ADTFunction[G]], args: Seq[Expr[G]])(implicit val o: Origin) extends Apply[G] with ADTFunctionInvocationImpl[G]
final case class ProverFunctionInvocation[G](ref: Ref[G, ProverFunction[G]], args: Seq[Expr[G]])(implicit val o: Origin) extends Apply[G] with ProverFunctionInvocationImpl[G]

sealed trait ApplyInlineable[G] extends Apply[G] with ApplyInlineableImpl[G]
sealed trait InstanceApply[G] extends Node[G] with InstanceApplyImpl[G]

sealed trait ApplyAnyPredicate[G] extends ApplyInlineable[G] with ApplyAnyPredicateImpl[G]
final case class PredicateApply[G](ref: Ref[G, Predicate[G]], args: Seq[Expr[G]], perm: Expr[G])(implicit val o: Origin) extends ApplyAnyPredicate[G] with PredicateApplyImpl[G]
final case class InstancePredicateApply[G](obj: Expr[G], ref: Ref[G, InstancePredicate[G]], args: Seq[Expr[G]], perm: Expr[G])(implicit val o: Origin) extends ApplyAnyPredicate[G] with InstanceApply[G] with InstancePredicateApplyImpl[G]
final case class CoalesceInstancePredicateApply[G](obj: Expr[G], ref: Ref[G, InstancePredicate[G]], args: Seq[Expr[G]], perm: Expr[G])(implicit val o: Origin) extends ApplyAnyPredicate[G] with InstanceApply[G] with CoalesceInstancePredicateApplyImpl[G]

sealed trait InvokingNode[G] extends Node[G] with InvokingNodeImpl[G]
sealed trait Invocation[G] extends ApplyInlineable[G] with InvokingNode[G] with InvocationImpl[G]

sealed trait AnyMethodInvocation[G] extends Invocation[G] with AnyMethodInvocationImpl[G]
final case class ProcedureInvocation[G](ref: Ref[G, Procedure[G]], args: Seq[Expr[G]], outArgs: Seq[Expr[G]], typeArgs: Seq[Type[G]], givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Expr[G], Ref[G, Variable[G]])])(val blame: Blame[InvocationFailure])(implicit val o: Origin) extends AnyMethodInvocation[G] with ProcedureInvocationImpl[G]
final case class MethodInvocation[G](obj: Expr[G], ref: Ref[G, InstanceMethod[G]], args: Seq[Expr[G]], outArgs: Seq[Expr[G]], typeArgs: Seq[Type[G]], givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Expr[G], Ref[G, Variable[G]])])(val blame: Blame[InstanceInvocationFailure])(implicit val o: Origin) extends AnyMethodInvocation[G] with InstanceApply[G] with MethodInvocationImpl[G]
final case class ConstructorInvocation[G](ref: Ref[G, Constructor[G]], classTypeArgs: Seq[Type[G]], args: Seq[Expr[G]], outArgs: Seq[Expr[G]], typeArgs: Seq[Type[G]], givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Expr[G], Ref[G, Variable[G]])])(val blame: Blame[InvocationFailure])(implicit val o: Origin) extends AnyMethodInvocation[G] with ConstructorInvocationImpl[G]

sealed trait AnyFunctionInvocation[G] extends Invocation[G] with AnyFunctionInvocationImpl[G]
final case class FunctionInvocation[G](ref: Ref[G, Function[G]], args: Seq[Expr[G]], typeArgs: Seq[Type[G]], givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Expr[G], Ref[G, Variable[G]])])(val blame: Blame[InvocationFailure])(implicit val o: Origin) extends AnyFunctionInvocation[G] with FunctionInvocationImpl[G]
final case class InstanceFunctionInvocation[G](obj: Expr[G], ref: Ref[G, InstanceFunction[G]], args: Seq[Expr[G]], typeArgs: Seq[Type[G]], givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Expr[G], Ref[G, Variable[G]])])(val blame: Blame[InstanceInvocationFailure])(implicit val o: Origin) extends AnyFunctionInvocation[G] with InstanceApply[G] with InstanceFunctionInvocationImpl[G]

sealed trait UnExpr[G] extends Expr[G] with UnExprImpl[G]

final case class UMinus[G](arg: Expr[G])(implicit val o: Origin) extends UnExpr[G] with UMinusImpl[G]
final case class BitNot[G](arg: Expr[G])(implicit val o: Origin) extends UnExpr[G] with BitNotImpl[G]
final case class Not[G](arg: Expr[G])(implicit val o: Origin) extends UnExpr[G] with NotImpl[G]

sealed trait BinExpr[G] extends Expr[G] with BinExprImpl[G]
sealed trait NumericBinExpr[G] extends BinExpr[G] with NumericBinExprImpl[G]

sealed trait DividingExpr[G] extends Expr[G] with DividingExprImpl[G]

final case class AmbiguousMult[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with AmbiguousMultImpl[G]
final case class AmbiguousPlus[G](left: Expr[G], right: Expr[G])(val blame: Blame[FrontendAdditiveError])(implicit val o: Origin) extends BinExpr[G] with AmbiguousPlusImpl[G]
final case class AmbiguousMinus[G](left: Expr[G], right: Expr[G])(val blame: Blame[FrontendAdditiveError])(implicit val o: Origin) extends BinExpr[G] with AmbiguousMinusImpl[G]
final case class AmbiguousOr[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with AmbiguousOrImpl[G]

sealed trait AmbiguousDividingExpr[G] extends DividingExpr[G] with BinExpr[G] with AmbiguousDividingExprImpl[G]
final case class AmbiguousDiv[G](left: Expr[G], right: Expr[G])(val blame: Blame[DivByZero])(implicit val o: Origin) extends BinExpr[G] with AmbiguousDividingExpr[G] with AmbiguousDivImpl[G]
final case class AmbiguousMod[G](left: Expr[G], right: Expr[G])(val blame: Blame[DivByZero])(implicit val o: Origin) extends BinExpr[G] with AmbiguousDividingExpr[G] with AmbiguousModImpl[G]
final case class AmbiguousTruncDiv[G](left: Expr[G], right: Expr[G])(val blame: Blame[DivByZero])(implicit val o: Origin) extends BinExpr[G] with AmbiguousDividingExpr[G] with AmbiguousTruncDivImpl[G]
final case class AmbiguousTruncMod[G](left: Expr[G], right: Expr[G])(val blame: Blame[DivByZero])(implicit val o: Origin) extends BinExpr[G] with AmbiguousDividingExpr[G] with AmbiguousTruncModImpl[G]

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
final case class RatDiv[G](left: Expr[G], right: Expr[G])(val blame: Blame[DivByZero])(implicit val o: Origin) extends BinExpr[G] with DividingExpr[G] with RatDivImpl[G]
final case class FloatDiv[G](left: Expr[G], right: Expr[G])(val blame: Blame[DivByZero])(implicit val o: Origin) extends BinExpr[G] with DividingExpr[G] with FloatDivImpl[G]
final case class FloorDiv[G](left: Expr[G], right: Expr[G])(val blame: Blame[DivByZero])(implicit val o: Origin) extends BinExpr[G] with DividingExpr[G] with FloorDivImpl[G]
final case class Mod[G](left: Expr[G], right: Expr[G])(val blame: Blame[DivByZero])(implicit val o: Origin) extends BinExpr[G] with DividingExpr[G] with ModImpl[G]
final case class TruncDiv[G](left: Expr[G], right: Expr[G])(val blame: Blame[DivByZero])(implicit val o: Origin) extends BinExpr[G] with DividingExpr[G] with TruncDivImpl[G]
final case class TruncMod[G](left: Expr[G], right: Expr[G])(val blame: Blame[DivByZero])(implicit val o: Origin) extends BinExpr[G] with DividingExpr[G] with TruncModImpl[G]

sealed trait VectorBinExpr[G] extends BinExpr[G] with VectorBinExprImpl[G]
final case class VectorPlus[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends VectorBinExpr[G] with VectorPlusImpl[G]
final case class VectorMinus[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends VectorBinExpr[G] with VectorMinusImpl[G]
final case class VectorMult[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends VectorBinExpr[G] with VectorMultImpl[G]
final case class VectorEq[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends VectorBinExpr[G] with VectorEqImpl[G]
final case class VectorNeq[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends VectorBinExpr[G] with VectorNeqImpl[G]

sealed trait DividingVectorBinExpr[G] extends VectorBinExpr[G] with DividingExpr[G]
final case class VectorFloorDiv[G](left: Expr[G], right: Expr[G])(val blame: Blame[DivByZero])(implicit val o: Origin) extends DividingVectorBinExpr[G] with VectorFloorDivImpl[G]
final case class VectorFloatDiv[G](left: Expr[G], right: Expr[G])(val blame: Blame[DivByZero])(implicit val o: Origin) extends DividingVectorBinExpr[G] with VectorFloatDivImpl[G]
final case class VectorTruncDiv[G](left: Expr[G], right: Expr[G])(val blame: Blame[DivByZero])(implicit val o: Origin) extends DividingVectorBinExpr[G] with VectorTruncDivImpl[G]
final case class VectorMod[G](left: Expr[G], right: Expr[G])(val blame: Blame[DivByZero])(implicit val o: Origin) extends DividingVectorBinExpr[G] with VectorModImpl[G]
final case class VectorTruncMod[G](left: Expr[G], right: Expr[G])(val blame: Blame[DivByZero])(implicit val o: Origin) extends DividingVectorBinExpr[G] with VectorTruncModImpl[G]

final case class StringConcat[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends BinExpr[G] with StringConcatImpl[G]

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
final case class ScaleByParBlock[G](block: Ref[G, ParBlockDecl[G]], res: Expr[G])(implicit val o: Origin) extends Expr[G] with ScaleByParBlockImpl[G]
final case class PolarityDependent[G](onInhale: Expr[G], onExhale: Expr[G])(implicit val o: Origin) extends Expr[G] with PolarityDependentImpl[G]

final case class Unfolding[G](res: Expr[G], body: Expr[G])(val blame: Blame[UnfoldFailed])(implicit val o: Origin) extends Expr[G] with UnfoldingImpl[G]

@family sealed trait Location[G] extends NodeFamily[G] with LocationImpl[G]
final case class HeapVariableLocation[G](ref: Ref[G, HeapVariable[G]])(implicit val o: Origin) extends Location[G] with HeapVariableLocationImpl[G]
final case class FieldLocation[G](obj: Expr[G], field: Ref[G, InstanceField[G]])(implicit val o: Origin) extends Location[G] with FieldLocationImpl[G]
final case class ModelLocation[G](obj: Expr[G], field: Ref[G, ModelField[G]])(implicit val o: Origin) extends Location[G] with ModelLocationImpl[G]
final case class SilverFieldLocation[G](obj: Expr[G], field: Ref[G, SilverField[G]])(implicit val o: Origin) extends Location[G] with SilverFieldLocationImpl[G]
final case class ArrayLocation[G](array: Expr[G], subscript: Expr[G])(val blame: Blame[ArrayLocationError])(implicit val o: Origin) extends Location[G] with ArrayLocationImpl[G]
final case class PointerLocation[G](pointer: Expr[G])(val blame: Blame[PointerLocationError])(implicit val o: Origin) extends Location[G] with PointerLocationImpl[G]
final case class PredicateLocation[G](predicate: Ref[G, Predicate[G]], args: Seq[Expr[G]])(implicit val o: Origin) extends Location[G] with PredicateLocationImpl[G]
final case class InstancePredicateLocation[G](predicate: Ref[G, InstancePredicate[G]], obj: Expr[G], args: Seq[Expr[G]])(implicit val o: Origin) extends Location[G] with InstancePredicateLocationImpl[G]
final case class AmbiguousLocation[G](expr: Expr[G])(val blame: Blame[PointerLocationError])(implicit val o: Origin) extends Location[G] with AmbiguousLocationImpl[G]
final case class InLinePatternLocation[G](loc: Location[G], pattern: Expr[G])(implicit val o: Origin) extends Location[G] with InLinePatternLocationImpl[G]

final case class Perm[G](loc: Location[G], perm: Expr[G])(implicit val o: Origin) extends Expr[G] with PermImpl[G]
final case class PointsTo[G](loc: Location[G], perm: Expr[G], value: Expr[G])(implicit val o: Origin) extends Expr[G] with PointsToImpl[G]
final case class CurPerm[G](loc: Location[G])(implicit val o: Origin) extends Expr[G] with CurPermImpl[G]

final case class Value[G](loc: Location[G])(implicit val o: Origin) extends Expr[G] with ValueImpl[G]

final case class ValidArray[G](arr: Expr[G], len: Expr[G])(implicit val o: Origin) extends Expr[G] with ValidArrayImpl[G]
final case class ValidMatrix[G](mat: Expr[G], w: Expr[G], h: Expr[G])(implicit val o: Origin) extends Expr[G] with ValidMatrixImpl[G]

final case class PermPointer[G](p: Expr[G], len: Expr[G], perm: Expr[G])(implicit val o: Origin) extends Expr[G] with PermPointerImpl[G]
final case class PermPointerIndex[G](p: Expr[G], idx: Expr[G], perm: Expr[G])(implicit val o: Origin) extends Expr[G] with PermPointerIndexImpl[G]

final case class ResourceOfResourceValue[G](res: Expr[G])(implicit val o: Origin) extends Expr[G] with ResourceOfResourceValueImpl[G]
final case class ResourceValue[G](res: Expr[G])(implicit val o: Origin) extends Expr[G] with ResourceValueImpl[G]

sealed trait Comparison[G] extends BinExpr[G] with ComparisonImpl[G]
sealed trait AmbiguousComparison[G] extends Comparison[G] with AmbiguousComparisonImpl[G]
final case class AmbiguousEq[G](left: Expr[G], right: Expr[G], vectorInnerType: Type[G])(implicit val o: Origin) extends AmbiguousComparison[G] with AmbiguousEqImpl[G]
final case class AmbiguousNeq[G](left: Expr[G], right: Expr[G], vectorInnerType: Type[G])(implicit val o: Origin) extends AmbiguousComparison[G] with AmbiguousNeqImpl[G]

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
final case class NewArray[G](element: Type[G], dims: Seq[Expr[G]], moreDims: Int, initialize: Boolean)(val blame: Blame[ArraySizeError])(implicit val o: Origin) extends Expr[G] with NewArrayImpl[G]
final case class NewPointerArray[G](element: Type[G], size: Expr[G])(val blame: Blame[ArraySizeError])(implicit val o: Origin) extends Expr[G] with NewPointerArrayImpl[G]
final case class FreePointer[G](pointer: Expr[G])(val blame: Blame[PointerFreeError])(implicit val o: Origin) extends Expr[G] with FreePointerImpl[G]
final case class Old[G](expr: Expr[G], at: Option[Ref[G, LabelDecl[G]]])(val blame: Blame[LabelNotReached])(implicit val o: Origin) extends Expr[G] with OldImpl[G]
final case class AmbiguousSubscript[G](collection: Expr[G], index: Expr[G])(val blame: Blame[FrontendSubscriptError])(implicit val o: Origin) extends Expr[G] with AmbiguousSubscriptImpl[G]
final case class SeqSubscript[G](seq: Expr[G], index: Expr[G])(val blame: Blame[SeqBoundFailure])(implicit val o: Origin) extends Expr[G] with SeqSubscriptImpl[G]
final case class VectorSubscript[G](seq: Expr[G], index: Expr[G])(val blame: Blame[VectorBoundFailure])(implicit val o: Origin) extends Expr[G] with VectorSubscriptImpl[G]
final case class ArraySubscript[G](arr: Expr[G], index: Expr[G])(val blame: Blame[ArraySubscriptError])(implicit val o: Origin) extends Expr[G] with ArraySubscriptImpl[G]
final case class PointerSubscript[G](pointer: Expr[G], index: Expr[G])(val blame: Blame[PointerSubscriptError])(implicit val o: Origin) extends Expr[G] with PointerSubscriptImpl[G]
final case class Length[G](arr: Expr[G])(val blame: Blame[ArrayNull])(implicit val o: Origin) extends Expr[G] with LengthImpl[G]
final case class Size[G](obj: Expr[G])(implicit val o: Origin) extends Expr[G] with SizeImpl[G]
final case class PointerBlockLength[G](pointer: Expr[G])(val blame: Blame[PointerNull])(implicit val o: Origin) extends Expr[G] with PointerBlockLengthImpl[G]
final case class PointerBlockOffset[G](pointer: Expr[G])(val blame: Blame[PointerNull])(implicit val o: Origin) extends Expr[G] with PointerBlockOffsetImpl[G]
final case class PointerLength[G](pointer: Expr[G])(val blame: Blame[PointerNull])(implicit val o: Origin) extends Expr[G] with PointerLengthImpl[G]
final case class SharedMemSize[G](pointer: Expr[G])(implicit val o: Origin) extends Expr[G] with SharedMemSizeImpl[G]
final case class NdIndex[G](indices: Seq[Expr[G]], dimensions: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with NdIndexImpl[G]
final case class NdPartialIndex[G](indices: Seq[Expr[G]], linearIndex: Expr[G], dimensions: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with NdPartialIndexImpl[G]
final case class NdLength[G](dimensions: Seq[Expr[G]])(implicit val o: Origin) extends Expr[G] with NdLengthImpl[G]

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
final case class Choose[G](xs: Expr[G])(val blame: Blame[SetEmpty])(implicit val o: Origin) extends Expr[G] with ChooseImpl[G]
final case class ChooseFresh[G](xs: Expr[G])(val blame: Blame[SetEmpty])(implicit val o: Origin) extends Expr[G] with ChooseFreshImpl[G]

final case class AmbiguousMember[G](x: Expr[G], xs: Expr[G])(implicit val o: Origin) extends Expr[G] with AmbiguousMemberImpl[G]
final case class SetMember[G](x: Expr[G], xs: Expr[G])(implicit val o: Origin) extends Expr[G] with SetMemberImpl[G]
final case class SeqMember[G](x: Expr[G], xs: Expr[G])(implicit val o: Origin) extends Expr[G] with SeqMemberImpl[G]
final case class MapMember[G](x: Expr[G], xs: Expr[G])(implicit val o: Origin) extends Expr[G] with MapMemberImpl[G]
final case class BagMemberCount[G](x: Expr[G], xs: Expr[G])(implicit val o: Origin) extends Expr[G] with BagMemberCountImpl[G]

final case class Permutation[G](xs: Expr[G], ys: Expr[G])(implicit val o: Origin) extends Expr[G] with PermutationImpl[G]
final case class OptEmpty[G](opt: Expr[G])(implicit val o: Origin) extends Expr[G] with OptEmptyImpl[G]
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

final case class CastFloat[G](e: Expr[G], t: Type[G] /* TFloat or TInt */)(implicit val o: Origin) extends Expr[G] with CastFloatImpl[G]

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
final case class Committed[G](obj: Expr[G])(val blame: Blame[LockObjectNull])(implicit val o: Origin) extends Expr[G] with CommittedImpl[G]
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

sealed trait SmtlibType[G] extends Type[G]
case class TSmtlibArray[G](index: Seq[Type[G]], value: Type[G])(implicit val o: Origin = DiagnosticOrigin) extends SmtlibType[G] with TSmtlibArrayImpl[G]
case class TSmtlibBitVector[G](size: Int)(implicit val o: Origin = DiagnosticOrigin) extends SmtlibType[G] with TSmtlibBitVectorImpl[G]
case class TSmtlibRoundingMode[G]()(implicit val o: Origin = DiagnosticOrigin) extends SmtlibType[G] with TSmtlibRoundingModeImpl[G]
case class TSmtlibFloatingPoint[G](exponentBits: Int, mantissaAndSignBits: Int)(implicit val o: Origin = DiagnosticOrigin) extends SmtlibType[G] with TSmtlibFloatingPointImpl[G]
case class TSmtlibString[G]()(implicit val o: Origin = DiagnosticOrigin) extends SmtlibType[G] with TSmtlibStringImpl[G]
case class TSmtlibRegLan[G]()(implicit val o: Origin = DiagnosticOrigin) extends SmtlibType[G] with TSmtlibRegLanImpl[G]
// Non-standard Z3 extensions
case class TSmtlibSeq[G](element: Type[G])(implicit val o: Origin = DiagnosticOrigin) extends SmtlibType[G] with TSmtlibSeqImpl[G]

@family sealed trait SmtlibFunctionSymbol[G] extends NodeFamily[G] with SmtlibFunctionSymbolImpl[G]
case class SmtlibADTFunctionSymbol[G](ref: Ref[G, ADTFunction[G]])(implicit val o: Origin) extends SmtlibFunctionSymbol[G] with SmtlibADTFunctionSymbolImpl[G]
case class SmtlibProverFunctionSymbol[G](ref: Ref[G, ProverFunction[G]])(implicit val o: Origin) extends SmtlibFunctionSymbol[G] with SmtlibProverFunctionSymbolImpl[G]

sealed trait SmtlibExpr[G] extends Expr[G]
case class SmtlibSelect[G](arr: Expr[G], is: Seq[Expr[G]])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibSelectImpl[G]
case class SmtlibStore[G](arr: Expr[G], is: Seq[Expr[G]], x: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStoreImpl[G]

case class SmtlibBitvecLiteral[G](data: BitString)(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibBitvecLiteralImpl[G]
case class SmtlibConcat[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibConcatImpl[G]
case class SmtlibExtract[G](inclusiveEndIndexFromRight: Int, startIndexFromRight: Int, bv: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibExtractImpl[G]
case class SmtlibBvNot[G](bv: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibBvNotImpl[G]
case class SmtlibBvAnd[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibBvAndImpl[G]
case class SmtlibBvOr[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibBvOrImpl[G]
case class SmtlibBvNeg[G](bv: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibBvNegImpl[G]
case class SmtlibBvAdd[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibBvAddImpl[G]
case class SmtlibBvMul[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibBvMulImpl[G]
case class SmtlibBvUDiv[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibBvUDivImpl[G]
case class SmtlibBvURem[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibBvURemImpl[G]
case class SmtlibBvShl[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibBvShlImpl[G]
case class SmtlibBvShr[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibBvShrImpl[G]
case class SmtlibBvULt[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibBvULtImpl[G]

case class SmtlibRNE[G]()(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibRNEImpl[G]
case class SmtlibRNA[G]()(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibRNAImpl[G]
case class SmtlibRTP[G]()(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibRTPImpl[G]
case class SmtlibRTN[G]()(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibRTNImpl[G]
case class SmtlibRTZ[G]()(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibRTZImpl[G]

case class SmtlibFp[G](sign: Expr[G], exponent: Expr[G], mantissa: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpImpl[G]
case class SmtlibFpAbs[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpAbsImpl[G]
case class SmtlibFpNeg[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpNegImpl[G]
case class SmtlibFpAdd[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpAddImpl[G]
case class SmtlibFpSub[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpSubImpl[G]
case class SmtlibFpMul[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpMulImpl[G]
case class SmtlibFpDiv[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpDivImpl[G]
case class SmtlibFpFma[G](left: Expr[G], right: Expr[G], addend: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpFmaImpl[G]
case class SmtlibFpSqrt[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpSqrtImpl[G]
case class SmtlibFpRem[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpRemImpl[G]
case class SmtlibFpRoundToIntegral[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpRoundToIntegralImpl[G]
case class SmtlibFpMin[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpMinImpl[G]
case class SmtlibFpMax[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpMaxImpl[G]
case class SmtlibFpLeq[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpLeqImpl[G]
case class SmtlibFpLt[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpLtImpl[G]
case class SmtlibFpGeq[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpGeqImpl[G]
case class SmtlibFpGt[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpGtImpl[G]
case class SmtlibFpEq[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpEqImpl[G]
case class SmtlibFpIsNormal[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpIsNormalImpl[G]
case class SmtlibFpIsSubnormal[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpIsSubnormalImpl[G]
case class SmtlibFpIsZero[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpIsZeroImpl[G]
case class SmtlibFpIsInfinite[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpIsInfiniteImpl[G]
case class SmtlibFpIsNaN[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpIsNaNImpl[G]
case class SmtlibFpIsNegative[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpIsNegativeImpl[G]
case class SmtlibFpIsPositive[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpIsPositiveImpl[G]
case class SmtlibToFp[G](bv: Expr[G], exponentBits: Int, mantissaAndSignBits: Int)(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibToFpImpl[G]
case class SmtlibFpCast[G](arg: Expr[G], exponentBits: Int, mantissaAndSignBits: Int)(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpCastImpl[G]
case class SmtlibFpFromReal[G](arg: Expr[G], exponentBits: Int, mantissaAndSignBits: Int)(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpFromRealImpl[G]
case class SmtlibFpFromSInt[G](bv: Expr[G], exponentBits: Int, mantissaAndSignBits: Int)(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpFromSIntImpl[G]
case class SmtlibFpFromUInt[G](bv: Expr[G], exponentBits: Int, mantissaAndSignBits: Int)(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpFromUIntImpl[G]
case class SmtlibFpToReal[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpToRealImpl[G]
case class SmtlibFpToSInt[G](arg: Expr[G], bits: Int)(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpToSIntImpl[G]
case class SmtlibFpToUInt[G](arg: Expr[G], bits: Int)(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibFpToUIntImpl[G]

case class SmtlibIsInt[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibIsIntImpl[G]
case class SmtlibToInt[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibToIntImpl[G]
case class SmtlibToReal[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibToRealImpl[G]
case class SmtlibPow[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with BinExpr[G] with SmtlibPowImpl[G]

case class SmtlibLiteralString[G](data: String)(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibLiteralStringImpl[G]
case class SmtlibStrConcat[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStrConcatImpl[G]
case class SmtlibStrLen[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStrLenImpl[G]
case class SmtlibStrLt[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStrLtImpl[G]
case class SmtlibStrLeq[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStrLeqImpl[G]
case class SmtlibStrAt[G](str: Expr[G], i: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStrAtImpl[G]
case class SmtlibSubstr[G](str: Expr[G], i: Expr[G], n: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibSubstrImpl[G]
case class SmtlibStrPrefixOf[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStrPrefixOfImpl[G]
case class SmtlibStrSuffixOf[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStrSuffixOfImpl[G]
case class SmtlibStrContains[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStrContainsImpl[G]
case class SmtlibStrIndexOf[G](haystack: Expr[G], needle: Expr[G], fromIndex: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStrIndexOfImpl[G]
case class SmtlibStrReplace[G](haystack: Expr[G], needle: Expr[G], replacement: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStrReplaceImpl[G]
case class SmtlibStrReplaceAll[G](haystack: Expr[G], needle: Expr[G], replacement: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStrReplaceAllImpl[G]
case class SmtlibStrReplaceRe[G](haystack: Expr[G], re: Expr[G], replacement: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStrReplaceReImpl[G]
case class SmtlibStrReplaceReAll[G](haystack: Expr[G], re: Expr[G], replacement: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStrReplaceReAllImpl[G]
case class SmtlibStrIsDigit[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStrIsDigitImpl[G]
case class SmtlibStrToCode[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStrToCodeImpl[G]
case class SmtlibStrFromCode[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStrFromCodeImpl[G]
case class SmtlibStrToInt[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStrToIntImpl[G]
case class SmtlibStrFromInt[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibStrFromIntImpl[G]

case class SmtlibReFromStr[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibReFromStrImpl[G]
case class SmtlibReContains[G](re: Expr[G], str: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibReContainsImpl[G]
case class SmtlibReNone[G]()(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibReNoneImpl[G]
case class SmtlibReAll[G]()(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibReAllImpl[G]
case class SmtlibReAllChar[G]()(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibReAllCharImpl[G]
case class SmtlibReConcat[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibReConcatImpl[G]
case class SmtlibReUnion[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibReUnionImpl[G]
case class SmtlibReInter[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibReInterImpl[G]
case class SmtlibReStar[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibReStarImpl[G]
case class SmtlibReComp[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibReCompImpl[G]
case class SmtlibReDiff[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibReDiffImpl[G]
case class SmtlibRePlus[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibRePlusImpl[G]
case class SmtlibReOpt[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibReOptImpl[G]
case class SmtlibReRange[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibReRangeImpl[G]
case class SmtlibReRepeat[G](count: Int, arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibReRepeatImpl[G]
case class SmtlibReRepeatRange[G](from: Int, to: Int, arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with SmtlibReRepeatRangeImpl[G]

// Non-standard Z3 extensions
case class Z3BvSub[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3BvSubImpl[G]
case class Z3BvSRem[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3BvSRemImpl[G]
case class Z3BvSMod[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3BvSModImpl[G]
case class Z3BvSShr[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3BvSShrImpl[G]
case class Z3BvNand[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3BvNandImpl[G]
case class Z3BvNor[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3BvNorImpl[G]
case class Z3BvXnor[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3BvXnorImpl[G]

case class Z3ArrayConst[G](domain: Seq[Type[G]], codomain: Type[G], value: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3ArrayConstImpl[G]
case class Z3ArrayOfFunction[G](ref: SmtlibFunctionSymbol[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3ArrayOfFunctionImpl[G]
case class Z3ArrayMap[G](ref: SmtlibFunctionSymbol[G], args: Seq[Expr[G]])(implicit val o: Origin) extends SmtlibExpr[G] with Z3ArrayMapImpl[G]

case class Z3SeqEmpty[G](elementType: Type[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3SeqEmptyImpl[G]
case class Z3SeqUnit[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3SeqUnitImpl[G]
case class Z3SeqConcat[G](left: Expr[G], right: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3SeqConcatImpl[G]
case class Z3SeqLen[G](arg: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3SeqLenImpl[G]
case class Z3SeqExtract[G](seq: Expr[G], offset: Expr[G], len: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3SeqExtractImpl[G]
case class Z3SeqAt[G](seq: Expr[G], offset: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3SeqAtImpl[G]
case class Z3SeqNth[G](seq: Expr[G], offset: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3SeqNthImpl[G]
case class Z3SeqContains[G](seq: Expr[G], subseq: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3SeqContainsImpl[G]
case class Z3SeqPrefixOf[G](pre: Expr[G], subseq: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3SeqPrefixOfImpl[G]
case class Z3SeqSuffixOf[G](post: Expr[G], seq: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3SeqSuffixOfImpl[G]
case class Z3SeqReplace[G](haystack: Expr[G], needle: Expr[G], replacement: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3SeqReplaceImpl[G]
case class Z3SeqMap[G](f: Expr[G], seq: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3SeqMapImpl[G]
case class Z3SeqMapI[G](f: Expr[G], offset: Expr[G], seq: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3SeqMapIImpl[G]
case class Z3SeqFoldl[G](f: Expr[G], base: Expr[G], seq: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3SeqFoldlImpl[G]
case class Z3SeqFoldlI[G](f: Expr[G], offset: Expr[G], base: Expr[G], seq: Expr[G])(implicit val o: Origin) extends SmtlibExpr[G] with Z3SeqFoldlIImpl[G]

case class Z3TransitiveClosure[G](ref: SmtlibFunctionSymbol[G], args: Seq[Expr[G]])(implicit val o: Origin) extends SmtlibExpr[G] with Z3TransitiveClosureImpl[G]

@family sealed trait CDeclarationSpecifier[G] extends NodeFamily[G] with CDeclarationSpecifierImpl[G]

sealed trait CSpecificationModifier[G] extends CDeclarationSpecifier[G] with CSpecificationModifierImpl[G]
final case class CPure[G]()(implicit val o: Origin) extends CSpecificationModifier[G] with CPureImpl[G]
final case class CInline[G]()(implicit val o: Origin) extends CSpecificationModifier[G] with CInlineImpl[G]

sealed trait CStorageClassSpecifier[G] extends CDeclarationSpecifier[G] with CStorageClassSpecifierImpl[G]
final case class CTypedef[G]()(implicit val o: Origin) extends CStorageClassSpecifier[G] with CTypedefImpl[G]
final case class CExtern[G]()(implicit val o: Origin) extends CStorageClassSpecifier[G] with CExternImpl[G]
final case class CStatic[G]()(implicit val o: Origin) extends CStorageClassSpecifier[G] with CStaticImpl[G]
final case class GPULocal[G]()(implicit val o: Origin) extends CStorageClassSpecifier[G] with GPULocalImpl[G]
final case class GPUGlobal[G]()(implicit val o: Origin) extends CStorageClassSpecifier[G] with GPUGlobalImpl[G]

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
final case class CFunctionTypeExtensionModifier[G](extensions: Seq[CTypeExtensions[G]])(implicit val o: Origin) extends CTypeSpecifier[G] with CFunctionTypeExtensionModifierImpl[G]

final case class CStructDeclaration[G](name: Option[String], decl: Seq[CStructMemberDeclarator[G]])(implicit val o: Origin) extends CTypeSpecifier[G] with CStructDeclarationImpl[G]
final case class CStructSpecifier[G](name: String)(implicit val o: Origin) extends CTypeSpecifier[G] with CStructSpecifierImpl[G] {
  var ref: Option[RefCStruct[G]] = None
}
@family final case class CStructMemberDeclarator[G](specs: Seq[CDeclarationSpecifier[G]], decls: Seq[CDeclarator[G]])(implicit val o: Origin) extends Declaration[G] with CStructMemberDeclaratorImpl[G]

final case class CTypeQualifierDeclarationSpecifier[G](typeQual: CTypeQualifier[G])(implicit val o: Origin) extends CDeclarationSpecifier[G] with CTypeQualifierDeclarationSpecifierImpl[G]

@family sealed trait CTypeQualifier[G] extends NodeFamily[G] with CTypeQualifierImpl[G]
final case class CConst[G]()(implicit val o: Origin) extends CTypeQualifier[G] with CConstImpl[G]
final case class CRestrict[G]()(implicit val o: Origin) extends CTypeQualifier[G] with CRestrictImpl[G]
final case class CVolatile[G]()(implicit val o: Origin) extends CTypeQualifier[G] with CVolatileImpl[G]
final case class CAtomic[G]()(implicit val o: Origin) extends CTypeQualifier[G] with CAtomicImpl[G]

sealed trait CFunctionSpecifier[G] extends CDeclarationSpecifier[G] with CFunctionSpecifierImpl[G]
sealed trait CAlignmentSpecifier[G] extends CDeclarationSpecifier[G] with CAlignmentSpecifierImpl[G]

sealed trait CGpgpuKernelSpecifier[G] extends CDeclarationSpecifier[G] with CGpgpuKernelSpecifierImpl[G]
final case class CUDAKernel[G]()(val blame: Blame[KernelFailure])(implicit val o: Origin) extends CGpgpuKernelSpecifier[G] with CUDAKernelImpl[G]
final case class OpenCLKernel[G]()(val blame: Blame[KernelFailure])(implicit val o: Origin) extends CGpgpuKernelSpecifier[G] with OpenCLKernelImpl[G]

@family final case class CPointer[G](qualifiers: Seq[CTypeQualifier[G]])(implicit val o: Origin) extends NodeFamily[G] with CPointerImpl[G]

@family final class CParam[G](val specifiers: Seq[CDeclarationSpecifier[G]], val declarator: CDeclarator[G])(implicit val o: Origin) extends Declaration[G] with CParamImpl[G]

@family sealed trait CDeclarator[G] extends NodeFamily[G] with CDeclaratorImpl[G]
final case class CPointerDeclarator[G](pointers: Seq[CPointer[G]], inner: CDeclarator[G])(implicit val o: Origin) extends CDeclarator[G] with CPointerDeclaratorImpl[G]
final case class CArrayDeclarator[G](qualifiers: Seq[CTypeQualifier[G]], size: Option[Expr[G]], inner: CDeclarator[G])(val blame: Blame[ArraySizeError])(implicit val o: Origin) extends CDeclarator[G] with CArrayDeclaratorImpl[G]
final case class CTypeExtensionDeclarator[G](extensions: Seq[CTypeExtensions[G]], inner: CDeclarator[G])(implicit val o: Origin) extends CDeclarator[G] with CTypeExtensionDeclaratorImpl[G]
final case class CTypedFunctionDeclarator[G](params: Seq[CParam[G]], varargs: Boolean, inner: CDeclarator[G])(implicit val o: Origin) extends CDeclarator[G] with CTypedFunctionDeclaratorImpl[G]
final case class CAnonymousFunctionDeclarator[G](params: Seq[String], inner: CDeclarator[G])(implicit val o: Origin) extends CDeclarator[G] with CAnonymousFunctionDeclaratorImpl[G]
final case class CName[G](name: String)(implicit val o: Origin) extends CDeclarator[G] with CNameImpl[G]

@family sealed trait CTypeExtensions[G] extends NodeFamily[G] with CTypeExtensionsImpl[G]
final case class CTypeAttribute[G](name: String, args: Seq[Expr[G]])(implicit val o: Origin) extends CTypeExtensions[G] with CTypeAttributeImpl[G]

@family final case class CInit[G](decl: CDeclarator[G], init: Option[Expr[G]])(implicit val o: Origin) extends NodeFamily[G] with CInitImpl[G] {
  var ref: Option[RefCFunctionDefinition[G]] = None
}

@family final case class CDeclaration[G](contract: ApplicableContract[G], kernelInvariant: Expr[G], specs: Seq[CDeclarationSpecifier[G]], inits: Seq[CInit[G]])(implicit val o: Origin) extends NodeFamily[G] with CDeclarationImpl[G]

final class CTranslationUnit[G](val declarations: Seq[GlobalDeclaration[G]])(implicit val o: Origin) extends GlobalDeclaration[G] with Declarator[G] with CTranslationUnitImpl[G]

sealed trait CAbstractDeclaration[G] extends GlobalDeclaration[G] with CAbstractDeclarationImpl[G]

@scopes[CLocalDeclaration] @scopes[CParam] final class CGlobalDeclaration[G](val decl: CDeclaration[G])(implicit val o: Origin) extends CAbstractDeclaration[G] with CGlobalDeclarationImpl[G]
@family final class CLocalDeclaration[G](val decl: CDeclaration[G])(implicit val o: Origin) extends Declaration[G] with CLocalDeclarationImpl[G]
@scopes[LabelDecl] @scopes[CLocalDeclaration] @scopes[CParam] final class CFunctionDefinition[G](val contract: ApplicableContract[G], val specs: Seq[CDeclarationSpecifier[G]], val declarator: CDeclarator[G], val body: Statement[G])(val blame: Blame[CallableFailure])(implicit val o: Origin) extends CAbstractDeclaration[G] with CFunctionDefinitionImpl[G] {
  var ref: Option[RefCGlobalDeclaration[G]] = None
}

sealed trait CStatement[G] extends Statement[G] with CStatementImpl[G]
final case class CDeclarationStatement[G](decl: CLocalDeclaration[G])(implicit val o: Origin) extends CStatement[G] with PurelySequentialStatement[G] with CDeclarationStatementImpl[G]
final case class CGoto[G](label: String)(implicit val o: Origin) extends CStatement[G] with CGotoImpl[G] {
  var ref: Option[LabelDecl[G]] = None
}

@family sealed trait GpuMemoryFence[G] extends NodeFamily[G] with GpuMemoryFenceImpl[G]
final case class GpuLocalMemoryFence[G]()(implicit val o: Origin) extends GpuMemoryFence[G] with GpuLocalMemoryFenceImpl[G]
final case class GpuGlobalMemoryFence[G]()(implicit val o: Origin) extends GpuMemoryFence[G] with GpuGlobalMemoryFenceImpl[G]
final case class GpuZeroMemoryFence[G](value: BigInt)(implicit val o: Origin) extends GpuMemoryFence[G] with GpuZeroMemoryFenceImpl[G]

final case class GpgpuBarrier[G](requires: Expr[G], ensures: Expr[G], specifiers: Seq[GpuMemoryFence[G]])(val blame: Blame[KernelBarrierFailure])(implicit val o: Origin) extends CStatement[G] with GpgpuBarrierImpl[G]
final case class GpgpuAtomic[G](impl: Statement[G], before: Statement[G], after: Statement[G])(implicit val o: Origin) extends CStatement[G] with GpgpuAtomicImpl[G]

sealed trait CExpr[G] extends Expr[G] with CExprImpl[G]
final case class CLocal[G](name: String)(val blame: Blame[DerefInsufficientPermission])(implicit val o: Origin) extends CExpr[G] with CLocalImpl[G] {
  var ref: Option[CNameTarget[G]] = None
}
final case class CInvocation[G](applicable: Expr[G], args: Seq[Expr[G]], givenArgs: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Expr[G], Ref[G, Variable[G]])])(val blame: Blame[FrontendInvocationError])(implicit val o: Origin) extends CExpr[G] with CInvocationImpl[G] {
  var ref: Option[CInvocationTarget[G]] = None
}
final case class CFieldAccess[G](obj: Expr[G], field: String)(val blame: Blame[FrontendDerefError])(implicit val o: Origin) extends CExpr[G] with CFieldAccessImpl[G] {
  var ref: Option[CDerefTarget[G]] = None
}
final case class CStructDeref[G](struct: Expr[G], field: String)(val blame: Blame[FrontendDerefError])(implicit val o: Origin) extends CExpr[G] with CStructDerefImpl[G] {
  var ref: Option[CDerefTarget[G]] = None
}
//TODO: Define real blame for a kernel invocation
final case class GpgpuCudaKernelInvocation[G](kernel: String, blocks: Expr[G], threads: Expr[G], args: Seq[Expr[G]], givenArgs: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Expr[G], Ref[G, Variable[G]])])(val blame: Blame[FrontendInvocationError])(implicit val o: Origin) extends CExpr[G] with GpgpuCudaKernelInvocationImpl[G] {
  var ref: Option[CInvocationTarget[G]] = None
}

final case class CCast[G](expr: Expr[G], castType: Type[G])(implicit val o: Origin) extends CExpr[G] with CCastImpl[G]

final case class CLiteralArray[G](exprs: Seq[Expr[G]])(implicit val o: Origin) extends CExpr[G] with CLiteralArrayImpl[G]

sealed trait CType[G] extends Type[G] with CTypeImpl[G]
final case class TCInt[G]()(implicit val o: Origin = DiagnosticOrigin) extends IntType[G] with CType[G] with TCIntImpl[G]
final case class TCFloat[G](exponent: Int, mantissa: Int)(implicit val o: Origin = DiagnosticOrigin) extends FloatType[G] with CType[G] with TCFloatImpl[G]
final case class CPrimitiveType[G](specifiers: Seq[CDeclarationSpecifier[G]])(implicit val o: Origin = DiagnosticOrigin) extends CType[G] with CPrimitiveTypeImpl[G]
final case class CTPointer[G](innerType: Type[G])(implicit val o: Origin = DiagnosticOrigin) extends CType[G] with CTPointerImpl[G]
final case class CTArray[G](size: Option[Expr[G]], innerType: Type[G])(val blame: Blame[ArraySizeError])(implicit val o: Origin = DiagnosticOrigin) extends CType[G] with CTArrayImpl[G]
final case class CTStruct[G](ref: Ref[G, CGlobalDeclaration[G]])(implicit val o: Origin = DiagnosticOrigin) extends CType[G] with CTStructImpl[G]
final case class CTVector[G](size: Expr[G], innerType: Type[G])(implicit val o: Origin = DiagnosticOrigin) extends CType[G] with CTVectorImpl[G]
final case class TOpenCLVector[G](size: BigInt, innerType: Type[G])(implicit val o: Origin = DiagnosticOrigin) extends CType[G]  with TOpenCLVectorImpl[G]
final case class CTCudaVec[G]()(implicit val o: Origin = DiagnosticOrigin) extends CType[G] with CTCudaVecImpl[G]

@family sealed trait CPPDeclarationSpecifier[G] extends NodeFamily[G] with CPPDeclarationSpecifierImpl[G]

sealed trait CPPSpecificationModifier[G] extends CPPDeclarationSpecifier[G] with CPPSpecificationModifierImpl[G]
final case class CPPPure[G]()(implicit val o: Origin) extends CPPSpecificationModifier[G] with CPPPureImpl[G]
final case class CPPInline[G]()(implicit val o: Origin) extends CPPSpecificationModifier[G] with CPPInlineImpl[G]

sealed trait CPPTypeSpecifier[G] extends CPPDeclarationSpecifier[G] with CPPTypeSpecifierImpl[G]
final case class CPPVoid[G]()(implicit val o: Origin) extends CPPTypeSpecifier[G] with CPPVoidImpl[G]
final case class CPPChar[G]()(implicit val o: Origin) extends CPPTypeSpecifier[G] with CPPCharImpl[G]
final case class CPPShort[G]()(implicit val o: Origin) extends CPPTypeSpecifier[G] with CPPShortImpl[G]
final case class CPPInt[G]()(implicit val o: Origin) extends CPPTypeSpecifier[G] with CPPIntImpl[G]
final case class CPPLong[G]()(implicit val o: Origin) extends CPPTypeSpecifier[G] with CPPLongImpl[G]
final case class CPPSigned[G]()(implicit val o: Origin) extends CPPTypeSpecifier[G] with CPPSignedImpl[G]
final case class CPPUnsigned[G]()(implicit val o: Origin) extends CPPTypeSpecifier[G] with CPPUnsignedImpl[G]
final case class CPPBool[G]()(implicit val o: Origin) extends CPPTypeSpecifier[G] with CPPBoolImpl[G]
final case class CPPTypedefName[G](var nestedName: String, genericArgs: Seq[CPPExprOrTypeSpecifier[G]])(implicit val o: Origin) extends CPPTypeSpecifier[G] with CPPTypedefNameImpl[G] {
  var ref: Option[CPPTypeNameTarget[G]] = None
}
final case class CPPSpecificationType[G](t: Type[G])(implicit val o: Origin) extends CPPTypeSpecifier[G] with CPPSpecificationTypeImpl[G]

@family sealed trait CPPAddressing[G] extends NodeFamily[G] with CPPAddressingImpl[G]
final case class CPPPointer[G]()(implicit val o: Origin) extends CPPAddressing[G] with CPPPointerImpl[G]
final case class CPPReference[G]()(implicit val o: Origin) extends CPPAddressing[G] with CPPReferenceImpl[G]

@family final class CPPParam[G](val specifiers: Seq[CPPDeclarationSpecifier[G]], val declarator: CPPDeclarator[G])(implicit val o: Origin) extends Declaration[G] with CPPParamImpl[G]

@family sealed trait CPPDeclarator[G] extends NodeFamily[G] with CPPDeclaratorImpl[G]
final case class CPPAddressingDeclarator[G](operators: Seq[CPPAddressing[G]], inner: CPPDeclarator[G])(implicit val o: Origin) extends CPPDeclarator[G] with CPPAddressingDeclaratorImpl[G]
final case class CPPArrayDeclarator[G](size: Option[Expr[G]], inner: CPPDeclarator[G])(val blame: Blame[ArraySizeError])(implicit val o: Origin) extends CPPDeclarator[G] with CPPArrayDeclaratorImpl[G]
final case class CPPTypedFunctionDeclarator[G](params: Seq[CPPParam[G]], varargs: Boolean, inner: CPPDeclarator[G])(implicit val o: Origin) extends CPPDeclarator[G] with CPPTypedFunctionDeclaratorImpl[G]
final case class CPPLambdaDeclarator[G](params: Seq[CPPParam[G]])(implicit val o: Origin) extends CPPDeclarator[G] with CPPLambdaDeclaratorImpl[G]
final case class CPPName[G](name: String)(implicit val o: Origin) extends CPPDeclarator[G] with CPPNameImpl[G]

@family final case class CPPInit[G](decl: CPPDeclarator[G], init: Option[Expr[G]])(implicit val o: Origin) extends NodeFamily[G] with CPPInitImpl[G] {
  var ref: Option[RefCPPFunctionDefinition[G]] = None
}

@family final case class CPPDeclaration[G](contract: ApplicableContract[G], specs: Seq[CPPDeclarationSpecifier[G]], inits: Seq[CPPInit[G]])(implicit val o: Origin) extends NodeFamily[G] with CPPDeclarationImpl[G]

final class CPPTranslationUnit[G](val declarations: Seq[GlobalDeclaration[G]])(implicit val o: Origin) extends GlobalDeclaration[G] with Declarator[G] with CPPTranslationUnitImpl[G]

sealed trait CPPAbstractDeclaration[G] extends GlobalDeclaration[G] with CPPAbstractDeclarationImpl[G]

@scopes[CPPLocalDeclaration] @scopes[CPPParam] final class CPPGlobalDeclaration[G](val decl: CPPDeclaration[G])(implicit val o: Origin) extends CPPAbstractDeclaration[G] with CPPGlobalDeclarationImpl[G]
@family final class CPPLocalDeclaration[G](val decl: CPPDeclaration[G])(implicit val o: Origin) extends Declaration[G] with CPPLocalDeclarationImpl[G]
@scopes[LabelDecl] @scopes[CPPLocalDeclaration] @scopes[CPPParam] final class CPPFunctionDefinition[G](val contract: ApplicableContract[G], val specs: Seq[CPPDeclarationSpecifier[G]], val declarator: CPPDeclarator[G], val body: Statement[G])(val blame: Blame[CallableFailure])(implicit val o: Origin) extends GlobalDeclaration[G] with CPPFunctionDefinitionImpl[G] {
  var ref: Option[RefCPPGlobalDeclaration[G]] = None
}

sealed trait CPPStatement[G] extends Statement[G] with CPPStatementImpl[G]
final case class CPPDeclarationStatement[G](decl: CPPLocalDeclaration[G])(implicit val o: Origin) extends CPPStatement[G] with PurelySequentialStatement[G] with CPPDeclarationStatementImpl[G]
final case class CPPLifetimeScope[G](body: Statement[G])(implicit val o: Origin) extends CPPStatement[G] with ControlContainerStatement[G] with CPPLifetimeScopeImpl[G]

sealed trait CPPExpr[G] extends Expr[G] with CPPExprImpl[G]
final case class CPPLocal[G](name: String, genericArgs: Seq[CPPExprOrTypeSpecifier[G]])(val blame: Blame[DerefInsufficientPermission])(implicit val o: Origin) extends CPPExpr[G] with CPPLocalImpl[G] {
  var ref: Option[CPPNameTarget[G]] = None
}
final case class CPPClassMethodOrFieldAccess[G](classInstance: Expr[G], methodOrFieldName: String)(val blame: Blame[FrontendDerefError])(implicit val o: Origin) extends CPPExpr[G] with CPPClassMethodOrFieldAccessImpl[G] {
  var ref: Option[CPPDerefTarget[G]] = None
}
final case class CPPInvocation[G](applicable: Expr[G], args: Seq[Expr[G]], givenArgs: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Expr[G], Ref[G, Variable[G]])])(val blame: Blame[FrontendInvocationError])(implicit val o: Origin) extends CPPExpr[G] with CPPInvocationImpl[G] {
  var ref: Option[CPPInvocationTarget[G]] = None
}
final case class CPPLambdaRef[G]()(implicit val o: Origin) extends CPPExpr[G] with CPPLambdaRefImpl[G]
final case class CPPLambdaDefinition[G](contract: ApplicableContract[G], declarator: CPPDeclarator[G], body: Statement[G])(val blame: Blame[SYCLKernelLambdaFailure])(implicit val o: Origin) extends CPPExpr[G] with CPPLambdaDefinitionImpl[G]

final case class CPPLiteralArray[G](exprs: Seq[Expr[G]])(implicit val o: Origin) extends CPPExpr[G] with CPPLiteralArrayImpl[G]

sealed trait CPPType[G] extends Type[G] with CPPTypeImpl[G]
final case class CPPPrimitiveType[G](specifiers: Seq[CPPDeclarationSpecifier[G]])(implicit val o: Origin = DiagnosticOrigin) extends CPPType[G] with CPPPrimitiveTypeImpl[G]
final case class CPPTArray[G](size: Option[Expr[G]], innerType: Type[G])(val blame: Blame[ArraySizeError])(implicit val o: Origin = DiagnosticOrigin) extends CPPType[G] with CPPTArrayImpl[G]
final case class CPPTLambda[G]()(implicit val o: Origin) extends CPPType[G] with CPPTLambdaImpl[G]

@family final case class CPPExprOrTypeSpecifier[G](expr: Option[Expr[G]], typeSpec: Option[CPPDeclarationSpecifier[G]])(implicit val o: Origin) extends NodeFamily[G] with CPPExprOrTypeSpecifierImpl[G]

final case class SYCLClassDefName[G](name: String, genericArgs: Seq[CPPExprOrTypeSpecifier[G]])(implicit val o: Origin) extends CPPTypeSpecifier[G] with SYCLClassDefNameImpl[G]

sealed trait SYCLTClass[G] extends Type[G] with SYCLTClassImpl[G]
sealed trait SYCLTConstructableClass[G] extends SYCLTClass[G] with SYCLTConstructableClassImpl[G]
final case class SYCLTEvent[G]()(implicit val o: Origin) extends SYCLTClass[G] with SYCLTEventImpl[G]
final case class SYCLTHandler[G]()(implicit val o: Origin) extends SYCLTClass[G] with SYCLTHandlerImpl[G]
final case class SYCLTQueue[G]()(implicit val o: Origin) extends SYCLTClass[G] with SYCLTQueueImpl[G]
final case class SYCLTItem[G](dimCount: Int = 1)(implicit val o: Origin) extends SYCLTClass[G] with SYCLTItemImpl[G]
final case class SYCLTNDItem[G](dimCount: Int = 1)(implicit val o: Origin) extends SYCLTClass[G] with SYCLTNDItemImpl[G]
final case class SYCLTRange[G](dimCount: Int = 1)(implicit val o: Origin) extends SYCLTConstructableClass[G] with SYCLTRangeImpl[G]
final case class SYCLTNDRange[G](dimCount: Int = 1)(implicit val o: Origin) extends SYCLTConstructableClass[G] with SYCLTNDRangeImpl[G]
final case class SYCLTBuffer[G](typ: Type[G] = TBool[G](), dimCount: Int = 1)(implicit val o: Origin) extends SYCLTConstructableClass[G] with SYCLTBufferImpl[G]
final case class SYCLTAccessor[G](typ: Type[G] = TBool[G](), dimCount: Int = 1, readOnly: Boolean = false)(implicit val o: Origin) extends SYCLTConstructableClass[G] with SYCLTAccessorImpl[G]
final case class SYCLTLocalAccessor[G](typ: Type[G] = TBool[G](), dimCount: Int = 1)(implicit val o: Origin) extends SYCLTConstructableClass[G] with SYCLTLocalAccessorImpl[G]
final case class SYCLTAccessMode[G]()(implicit val o: Origin) extends SYCLTClass[G] with SYCLTAccessModeImpl[G]

sealed trait SYCLClassObject[G] extends CPPExpr[G]
final case class SYCLRange[G](dimensions: Seq[Expr[G]])(implicit val o: Origin) extends SYCLClassObject[G] with SYCLRangeImpl[G]
final case class SYCLNDRange[G](globalSize: Expr[G], localSize: Expr[G])(implicit val o: Origin) extends SYCLClassObject[G] with SYCLNDRangeImpl[G]

sealed trait SYCLAccessMode[G] extends SYCLClassObject[G] with SYCLAccessModeImpl[G]
final case class SYCLReadWriteAccess[G]()(implicit val o: Origin) extends SYCLAccessMode[G] with SYCLReadWriteAccessImpl[G]
final case class SYCLReadOnlyAccess[G]()(implicit val o: Origin) extends SYCLAccessMode[G] with SYCLReadOnlyAccessImpl[G]

@family final case class JavaName[G](names: Seq[String])(implicit val o: Origin) extends NodeFamily[G] with JavaNameImpl[G] {
  var ref: Option[JavaTypeNameTarget[G]] = None
}
@family final case class JavaImport[G](isStatic: Boolean, name: JavaName[G], star: Boolean)(implicit val o: Origin) extends NodeFamily[G] with JavaImportImpl[G]

@family sealed trait JavaModifier[G] extends NodeFamily[G] with JavaModifierImpl[G]
final case class JavaPublic[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaPublicImpl[G]
final case class JavaProtected[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaProtectedImpl[G]
final case class JavaPrivate[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaPrivateImpl[G]
final case class JavaStatic[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaStaticImpl[G]
final case class JavaAbstract[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaAbstractImpl[G]
final case class JavaFinal[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaFinalImpl[G]
final case class JavaStrictFP[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaStrictFPImpl[G]
final case class JavaNative[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaNativeImpl[G]
final case class JavaSynchronized[G]()(val blame: Blame[LockRegionFailure])(implicit val o: Origin) extends JavaModifier[G] with JavaSynchronizedImpl[G]
final case class JavaTransient[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaTransientImpl[G]
final case class JavaVolatile[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaVolatileImpl[G]
final case class JavaAnnotation[G](name: Type[G], args: Seq[(String, Expr[G])]
                                  )(val blame: Blame[JavaAnnotationFailure]
                                  )(implicit val o: Origin) extends JavaModifier[G] with JavaAnnotationImpl[G] {
  var data: Option[JavaAnnotationData[G]] = None
}

final case class JavaPure[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaPureImpl[G]
final case class JavaInline[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaInlineImpl[G]
final case class JavaBipAnnotation[G]()(implicit val o: Origin) extends JavaModifier[G] with JavaBipAnnotationImpl[G]

@family final case class JavaVariableDeclaration[G](name: String, moreDims: Int, init: Option[Expr[G]])(implicit val o: Origin) extends NodeFamily[G] with JavaVariableDeclarationImpl[G]

sealed trait JavaGlobalDeclaration[G] extends GlobalDeclaration[G] with JavaGlobalDeclarationImpl[G]
final class JavaNamespace[G](val pkg: Option[JavaName[G]], val imports: Seq[JavaImport[G]], val declarations: Seq[GlobalDeclaration[G]])(implicit val o: Origin) extends JavaGlobalDeclaration[G] with Declarator[G] with JavaNamespaceImpl[G]

sealed trait JavaClassOrInterface[G] extends JavaGlobalDeclaration[G] with Declarator[G] with JavaClassOrInterfaceImpl[G]
@scopes[Variable] final class JavaClass[G](val name: String, val modifiers: Seq[JavaModifier[G]], val typeParams: Seq[Variable[G]], val intrinsicLockInvariant: Expr[G], val ext: Type[G], val imp: Seq[Type[G]], val decls: Seq[ClassDeclaration[G]])(val blame: Blame[JavaImplicitConstructorFailure])(implicit val o: Origin) extends JavaClassOrInterface[G] with JavaClassImpl[G]
@scopes[Variable] final class JavaInterface[G](val name: String, val modifiers: Seq[JavaModifier[G]], val typeParams: Seq[Variable[G]], val ext: Seq[Type[G]], val decls: Seq[ClassDeclaration[G]])(implicit val o: Origin) extends JavaClassOrInterface[G] with JavaInterfaceImpl[G]
final class JavaAnnotationInterface[G](val name: String, val modifiers: Seq[JavaModifier[G]], val ext: Type[G], val decls: Seq[ClassDeclaration[G]])(implicit val o: Origin) extends JavaClassOrInterface[G] with JavaAnnotationInterfaceImpl[G]

sealed trait JavaClassDeclaration[G] extends ClassDeclaration[G] with JavaClassDeclarationImpl[G]
final class JavaSharedInitialization[G](val isStatic: Boolean, val initialization: Statement[G])(implicit val o: Origin) extends JavaClassDeclaration[G] with JavaSharedInitializationImpl[G]
final class JavaFields[G](val modifiers: Seq[JavaModifier[G]], val t: Type[G], val decls: Seq[JavaVariableDeclaration[G]])(implicit val o: Origin) extends JavaClassDeclaration[G] with JavaFieldsImpl[G]
@scopes[LabelDecl] @scopes[JavaLocalDeclaration] @scopes[JavaParam] final class JavaConstructor[G](val modifiers: Seq[JavaModifier[G]], val name: String, val parameters: Seq[JavaParam[G]], val typeParameters: Seq[Variable[G]], val signals: Seq[Type[G]], val body: Statement[G], val contract: ApplicableContract[G])(val blame: Blame[JavaConstructorFailure])(implicit val o: Origin) extends JavaClassDeclaration[G] with JavaConstructorImpl[G]
@family final class JavaParam[G](val modifiers: Seq[JavaModifier[G]], val name: String, val t: Type[G])(implicit val o: Origin) extends Declaration[G] with JavaParamImpl[G]

@scopes[LabelDecl] @scopes[JavaLocalDeclaration] @scopes[JavaParam] final class JavaMethod[G](val modifiers: Seq[JavaModifier[G]], val returnType: Type[G], val dims: Int, val name: String, val parameters: Seq[JavaParam[G]], val typeParameters: Seq[Variable[G]], val signals: Seq[Type[G]], val body: Option[Statement[G]], val contract: ApplicableContract[G])(val blame: Blame[CallableFailure])(implicit val o: Origin) extends JavaClassDeclaration[G] with JavaMethodImpl[G]
@scopes[JavaParam] final class JavaAnnotationMethod[G](val returnType: Type[G], val name: String, val default: Option[Expr[G]])(implicit val o: Origin) extends JavaClassDeclaration[G] with JavaAnnotationMethodImpl[G]

@family final class JavaLocalDeclaration[G](val modifiers: Seq[JavaModifier[G]], val t: Type[G], val decls: Seq[JavaVariableDeclaration[G]])(implicit val o: Origin) extends Declaration[G] with JavaLocalDeclarationImpl[G]

sealed trait JavaStatement[G] extends Statement[G] with JavaStatementImpl[G]
final case class JavaLocalDeclarationStatement[G](decl: JavaLocalDeclaration[G])(implicit val o: Origin) extends JavaStatement[G] with PurelySequentialStatement[G] with JavaLocalDeclarationStatementImpl[G]

sealed trait JavaType[G] extends Type[G] with JavaTypeImpl[G]
final case class JavaNamedType[G](names: Seq[(String, Option[Seq[Type[G]]])])(implicit val o: Origin) extends JavaType[G] with JavaNamedTypeImpl[G] {
  var ref: Option[JavaTypeNameTarget[G]] = None
}
final case class JavaTClass[G](ref: Ref[G, JavaClassOrInterface[G]], typeArgs: Seq[Type[G]])(implicit val o: Origin = DiagnosticOrigin) extends JavaType[G] with JavaTClassImpl[G]

final case class JavaWildcard[G]()(implicit val o: Origin = DiagnosticOrigin) extends JavaType[G] with JavaWildcardImpl[G]

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
final case class JavaInvocation[G](obj: Option[Expr[G]], typeParams: Seq[Type[G]], method: String, arguments: Seq[Expr[G]], givenArgs: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Expr[G], Ref[G, Variable[G]])])(val blame: Blame[FrontendInvocationError])(implicit val o: Origin) extends JavaExpr[G] with JavaInvocationImpl[G] {
  var ref: Option[JavaInvocationTarget[G]] = None
}
final case class JavaNewClass[G](args: Seq[Expr[G]], typeArgs: Seq[Type[G]], name: Type[G], givenArgs: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Expr[G], Ref[G, Variable[G]])])(val blame: Blame[InvocationFailure])(implicit val o: Origin) extends JavaExpr[G] with JavaNewClassImpl[G] {
  var ref: Option[JavaConstructorTarget[G]] = None
}
final case class JavaNewLiteralArray[G](baseType: Type[G], dims: Int, initializer: Expr[G])(val blame: Blame[ArraySizeError])(implicit val o: Origin) extends JavaExpr[G] with JavaNewLiteralArrayImpl[G]
final case class JavaNewDefaultArray[G](baseType: Type[G], specifiedDims: Seq[Expr[G]], moreDims: Int)(val blame: Blame[ArraySizeError])(implicit val o: Origin) extends JavaExpr[G] with JavaNewDefaultArrayImpl[G]
final case class JavaStringValue[G](data: String, t: Type[G])(implicit val o: Origin) extends JavaExpr[G] with JavaStringValueImpl[G]

final class JavaBipGlueContainer[G](val job: Expr[G])(implicit val o: Origin) extends JavaGlobalDeclaration[G] with JavaBipGlueContainerImpl[G]

final case class JavaBipGlue[G](elems: Seq[JavaBipGlueElement[G]])(val blame: Blame[BipGlueFailure])(implicit val o: Origin) extends JavaExpr[G] with JavaBipGlueImpl[G]

@family final case class JavaBipGlueName[G](t: Type[G], e: Expr[G])(implicit val o: Origin) extends NodeFamily[G] with JavaBipGlueNameImpl[G] {
  var data: Option[(JavaClass[G], String)] = None
}
@family sealed trait JavaBipGlueElement[G] extends NodeFamily[G] with JavaBipGlueElementImpl[G]
final case class JavaBipGlueRequires[G](port: JavaBipGlueName[G], others: Seq[JavaBipGlueName[G]])(implicit val o: Origin) extends JavaBipGlueElement[G] with JavaBipGlueRequiresImpl[G]
final case class JavaBipGlueAccepts[G](port: JavaBipGlueName[G], others: Seq[JavaBipGlueName[G]])(implicit val o: Origin) extends JavaBipGlueElement[G] with JavaBipGlueAcceptsImpl[G]
final case class JavaBipGlueSynchron[G](port0: JavaBipGlueName[G], port1: JavaBipGlueName[G])(implicit val o: Origin) extends JavaBipGlueElement[G] with JavaBipGlueSynchronImpl[G]
final case class JavaBipGlueDataWire[G](dataOut: JavaBipGlueName[G], dataIn: JavaBipGlueName[G])(implicit val o: Origin) extends JavaBipGlueElement[G] with JavaBipGlueDataWireImpl[G]

final class BipGlue[G](val requires: Seq[BipGlueRequires[G]], val accepts: Seq[BipGlueAccepts[G]], val dataWires: Seq[BipGlueDataWire[G]])(val blame: Blame[BipGlueFailure])(implicit val o: Origin) extends GlobalDeclaration[G] with BipGlueImpl[G]
@family final case class BipGlueRequires[G](port: Ref[G, BipPort[G]], others: Seq[Ref[G, BipPort[G]]])(implicit val o: Origin) extends NodeFamily[G] with BipGlueRequiresImpl[G]
@family final case class BipGlueAccepts[G](port: Ref[G, BipPort[G]], others: Seq[Ref[G, BipPort[G]]])(implicit val o: Origin) extends NodeFamily[G] with BipGlueAcceptsImpl[G]
@family final case class BipGlueDataWire[G](dataOut: Ref[G, BipOutgoingData[G]], dataIn: Ref[G, BipIncomingData[G]])(implicit val o: Origin) extends NodeFamily[G] with BipGlueDataWireImpl[G]

sealed trait BipData[G] extends ClassDeclaration[G] with BipDataImpl[G]
final class BipIncomingData[G](val t: Type[G])(implicit val o: Origin) extends BipData[G] with BipIncomingDataImpl[G]
final class BipOutgoingData[G](val t: Type[G], val body: Statement[G], val pure: Boolean)(val blame: Blame[BipOutgoingDataPreconditionUnsatisfiable])(implicit val o: Origin) extends BipData[G] with BipOutgoingDataImpl[G]
final case class BipLocalIncomingData[G](ref: Ref[G, BipIncomingData[G]])(implicit val o: Origin) extends Expr[G] with BipLocalIncomingDataImpl[G]

final class BipStatePredicate[G](val expr: Expr[G])(implicit val o: Origin) extends ClassDeclaration[G] with BipStatePredicateImpl[G]
@family final case class BipTransitionSignature[G](portName: String, sourceStateName: String, targetStateName: String, textualGuard: Option[String])(implicit val o: Origin) extends NodeFamily[G] with BipTransitionSignatureImpl[G]
final class BipTransition[G](val signature: BipTransitionSignature[G],
                             val port: Ref[G, BipPort[G]],
                             val source: Ref[G, BipStatePredicate[G]], val target: Ref[G, BipStatePredicate[G]],
                             val data: Seq[Ref[G, BipIncomingData[G]]], val guard: Expr[G],
                             val requires: Expr[G], val ensures: Expr[G], val body: Statement[G]
                            )(val blame: Blame[BipTransitionFailure])(implicit val o: Origin) extends ClassDeclaration[G] with BipTransitionImpl[G]
final class BipGuard[G](val data: Seq[Ref[G, BipIncomingData[G]]], val body: Statement[G], val pure: Boolean)(val blame: Blame[BipGuardPreconditionUnsatisfiable])(implicit val o: Origin) extends ClassDeclaration[G] with BipGuardImpl[G]
final case class BipGuardInvocation[G](obj: Expr[G], guard: Ref[G, BipGuard[G]])(implicit val o: Origin) extends Expr[G] with BipGuardInvocationImpl[G]
final class BipComponent[G](val fqn: Seq[String], val invariant: Expr[G], val initial: Ref[G, BipStatePredicate[G]])(implicit val o: Origin) extends ClassDeclaration[G] with BipComponentImpl[G]
final class BipConstructor[G](val args: Seq[Variable[G]], val body: Statement[G], val requires: Expr[G])(val blame: Blame[BipConstructorFailure])(implicit val o: Origin) extends ClassDeclaration[G] with Declarator[G] with BipConstructorImpl[G]

final class BipPort[G](val t: BipPortType[G])(implicit val o: Origin) extends ClassDeclaration[G] with BipPortImpl[G]
@family sealed trait BipPortType[G] extends NodeFamily[G] with BipPortTypeImpl[G]
final case class BipEnforceable[G]()(implicit val o: Origin = DiagnosticOrigin) extends BipPortType[G] with BipEnforceableImpl[G]
final case class BipSpontaneous[G]()(implicit val o: Origin = DiagnosticOrigin) extends BipPortType[G] with BipSpontaneousImpl[G]
final case class BipInternal[G]()(implicit val o: Origin = DiagnosticOrigin) extends BipPortType[G] with BipInternalImpl[G]

final case class BipPortSynchronization[G](ports: Seq[Ref[G, BipPort[G]]], wires: Seq[BipGlueDataWire[G]])(val blame: Blame[BipSynchronizationFailure])(implicit val o: Origin) extends GlobalDeclaration[G] with BipPortSynchronizationImpl[G]
final case class BipTransitionSynchronization[G](transitions: Seq[Ref[G, BipTransition[G]]], wires: Seq[BipGlueDataWire[G]])(val blame: Blame[BipSynchronizationFailure])(implicit val o: Origin) extends GlobalDeclaration[G] with BipTransitionSynchronizationImpl[G]

@family final class LlvmFunctionContract[G](val value:String, val variableRefs:Seq[(String, Ref[G, Variable[G]])], val invokableRefs:Seq[(String, Ref[G, LlvmCallable[G]])])
                                   (val blame: Blame[NontrivialUnsatisfiable])
                                   (implicit val o: Origin) extends NodeFamily[G] with LlvmFunctionContractImpl[G] {
  var data: Option[ApplicableContract[G]] = None
}
sealed trait LlvmCallable[G] extends GlobalDeclaration[G]
@scopes[LabelDecl] final class LlvmFunctionDefinition[G](val returnType: Type[G],
                                      val args: Seq[Variable[G]],
                                      val functionBody: Statement[G],
                                      val contract: LlvmFunctionContract[G],
                                      val pure: Boolean = false)
                                     (val blame: Blame[CallableFailure])(implicit val o: Origin)
  extends LlvmCallable[G] with Applicable[G] with LlvmFunctionDefinitionImpl[G]
@scopes[LabelDecl] final class LlvmSpecFunction[G](val name: String, val returnType: Type[G], val args: Seq[Variable[G]], val typeArgs: Seq[Variable[G]],
                                val body: Option[Expr[G]], val contract: ApplicableContract[G], val inline: Boolean = false, val threadLocal: Boolean = false)
                               (val blame: Blame[ContractedFailure])(implicit val o: Origin)
  extends LlvmCallable[G] with AbstractFunction[G] with LlvmSpecFunctionImpl[G]
final case class LlvmFunctionInvocation[G](ref: Ref[G, LlvmFunctionDefinition[G]],
                                     args: Seq[Expr[G]],
                                     givenMap: Seq[(Ref[G, Variable[G]], Expr[G])],
                                     yields: Seq[(Expr[G], Ref[G, Variable[G]])])
                                    (val blame: Blame[InvocationFailure])(implicit val o: Origin) extends Apply[G] with LlvmFunctionInvocationImpl[G]
final case class LlvmLoop[G](cond:Expr[G], contract:LlvmLoopContract[G], body:Statement[G])
                       (implicit val o: Origin) extends CompositeStatement[G] with LlvmLoopImpl[G]
@family sealed trait LlvmLoopContract[G] extends NodeFamily[G] with LlvmLoopContractImpl[G]
final case class LlvmLoopInvariant[G](value:String, references:Seq[(String, Ref[G, Declaration[G]])])
                                     (val blame: Blame[LoopInvariantFailure])
                                     (implicit val o: Origin) extends LlvmLoopContract[G] with LlvmLoopInvariantImpl[G]
sealed trait LlvmExpr[G] extends Expr[G] with LlvmExprImpl[G]
final case class LlvmLocal[G](name: String)(val blame: Blame[DerefInsufficientPermission])(implicit val o: Origin) extends LlvmExpr[G] with LlvmLocalImpl[G] {
  var ref: Option[Ref[G, Variable[G]]] = None
}
final case class LlvmAmbiguousFunctionInvocation[G](name: String,
                                                    args: Seq[Expr[G]],
                                                    givenMap: Seq[(Ref[G, Variable[G]], Expr[G])],
                                                    yields: Seq[(Expr[G], Ref[G, Variable[G]])])
                                                   (val blame: Blame[InvocationFailure])(implicit val o: Origin) extends LlvmExpr[G] with LlvmAmbiguousFunctionInvocationImpl[G] {
  var ref: Option[Ref[G, LlvmCallable[G]]] = None
}

final class LlvmGlobal[G](val value: String)(implicit val o: Origin) extends GlobalDeclaration[G] with LlvmGlobalImpl[G] {
  var data: Option[Seq[GlobalDeclaration[G]]] = None
}
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
final case class PVLInvocation[G](obj: Option[Expr[G]], method: String, args: Seq[Expr[G]], typeArgs: Seq[Type[G]], givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Expr[G], Ref[G, Variable[G]])])(val blame: Blame[FrontendInvocationError])(implicit val o: Origin) extends PVLExpr[G] with PVLInvocationImpl[G] {
  var ref: Option[PVLInvocationTarget[G]] = None
}

final case class PVLNew[G](t: Type[G], typeArgs: Seq[Type[G]], args: Seq[Expr[G]], givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], yields: Seq[(Expr[G], Ref[G, Variable[G]])])(val blame: Blame[InvocationFailure])(implicit val o: Origin) extends PVLExpr[G] with PVLNewImpl[G] {
  var ref: Option[PVLConstructorTarget[G]] = None
}

sealed trait PVLClassDeclaration[G] extends ClassDeclaration[G] with PVLClassDeclarationImpl[G]
@scopes[LabelDecl] final class PVLConstructor[G](val contract: ApplicableContract[G], val typeArgs: Seq[Variable[G]], val args: Seq[Variable[G]], val body: Option[Statement[G]])(val blame: Blame[ConstructorFailure])(implicit val o: Origin) extends PVLClassDeclaration[G] with PVLConstructorImpl[G]

final case class TChoreography[G](cls: Ref[G, Choreography[G]])(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType[G] with TChoreographyImpl[G]
final case class TPVLChoreography[G](cls: Ref[G, PVLChoreography[G]])(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType[G] with TPVLChoreographyImpl[G]
final case class TEndpoint[G](cls: Ref[G, Endpoint[G]])(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType[G] with TEndpointImpl[G]

final class PVLEndpoint[G](val name: String, val cls: Ref[G, Class[G]], val typeArgs: Seq[Type[G]], val args: Seq[Expr[G]])(val blame: Blame[EndpointFailure])(implicit val o: Origin) extends ClassDeclaration[G] with PVLEndpointImpl[G] {
  var ref: Option[PVLConstructorTarget[G]] = None
}
final class PVLChoreography[G](val name: String, val declarations: Seq[ClassDeclaration[G]], val contract: ApplicableContract[G], val args: Seq[Variable[G]])(val blame: Blame[SeqCallableFailure])(implicit val o: Origin) extends GlobalDeclaration[G] with PVLChoreographyImpl[G] with Declarator[G]
final case class PVLChorRun[G](body: Statement[G], contract: ApplicableContract[G])(val blame: Blame[SeqCallableFailure])(implicit val o: Origin) extends ClassDeclaration[G] with PVLChorRunImpl[G]

@family case class PVLEndpointName[G](name: String)(implicit val o: Origin) extends PVLEndpointNameImpl[G] with NodeFamily[G] {
  var ref: Option[RefPVLEndpoint[G]] = None
}

// Resolution of invariant can depend on communicate's target/msg through \sender, \receiver, \msg. Therefore, definitions are nested like this,
// to ensure that PVLCommunicate is fully resolved before the invariant is typechecked.
final case class PVLChannelInvariant[G](comm: Statement[G], inv: Expr[G])(implicit val o: Origin) extends Statement[G] with PVLChannelInvariantImpl[G]
final case class PVLCommunicate[G](receiver: Option[PVLEndpointName[G]], target: Expr[G], sender: Option[PVLEndpointName[G]], msg: Expr[G])(val blame: Blame[PVLCommunicateFailure])(implicit val o: Origin) extends Statement[G] with PurelySequentialStatement[G] with PVLCommunicateImpl[G] {
  var inferredSender: Option[PVLEndpoint[G]] = None
  var inferredReceiver: Option[PVLEndpoint[G]] = None
}
final case class PVLChorStatement[G](endpoint: Option[PVLEndpointName[G]], inner: Statement[G])(val blame: Blame[ChorStatementFailure])(implicit val o: Origin) extends Statement[G] with PVLChorStatementImpl[G]
final case class PVLChorPerm[G](endpoint: PVLEndpointName[G], loc: Location[G], perm: Expr[G])(implicit val o: Origin) extends PVLExpr[G] with PVLChorPermImpl[G]
final case class PVLSender[G]()(implicit val o: Origin) extends Expr[G] with PVLSenderImpl[G] {
  var ref: Option[PVLCommunicate[G]] = None
}
final case class PVLReceiver[G]()(implicit val o: Origin) extends Expr[G] with PVLReceiverImpl[G] {
  var ref: Option[PVLCommunicate[G]] = None
}
final case class PVLMessage[G]()(implicit val o: Origin) extends Expr[G] with PVLMessageImpl[G] {
  var ref: Option[PVLCommunicate[G]] = None
}


@family final class Endpoint[G](val cls: Ref[G, Class[G]], val typeArgs: Seq[Type[G]], val constructor: Ref[G, Constructor[G]], val args: Seq[Expr[G]])(val blame: Blame[EndpointFailure])(implicit val o: Origin) extends Declaration[G] with EndpointImpl[G]
@scopes[Endpoint] final class Choreography[G](val contract: ApplicableContract[G], val params : Seq[Variable[G]], val endpoints: Seq[Endpoint[G]], val preRun: Option[Statement[G]], val run: ChorRun[G], val decls: Seq[ClassDeclaration[G]])(val blame: Blame[SeqCallableFailure])(implicit val o: Origin) extends GlobalDeclaration[G] with ChoreographyImpl[G]
@family final case class ChorRun[G](body: Statement[G], contract: ApplicableContract[G])(val blame: Blame[SeqCallableFailure])(implicit val o: Origin) extends NodeFamily[G] with ChorRunImpl[G]

@family final class Communicate[G](val invariant: Expr[G], val receiver: Option[Ref[G, Endpoint[G]]], val target: Expr[G], val sender: Option[Ref[G, Endpoint[G]]], val msg: Expr[G])(val blame: Blame[CommunicateFailure])(implicit val o: Origin) extends Declaration[G] with CommunicateImpl[G]
@scopes[Communicate] final case class CommunicateStatement[G](inner: Communicate[G])(implicit val o: Origin) extends PurelySequentialStatement[G] with CommunicateStatementImpl[G]

final case class EndpointName[G](ref: Ref[G, Endpoint[G]])(implicit val o: Origin) extends Expr[G] with EndpointNameImpl[G]
final case class ChorPerm[G](endpoint: Ref[G, Endpoint[G]], loc: Location[G], perm: Expr[G])(implicit val o: Origin) extends Expr[G] with ChorPermImpl[G]
final case class Sender[G](ref: Ref[G, Communicate[G]])(implicit val o: Origin) extends Expr[G] with SenderImpl[G]
final case class Receiver[G](ref: Ref[G, Communicate[G]])(implicit val o: Origin) extends Expr[G] with ReceiverImpl[G]
final case class Message[G](ref: Ref[G, Communicate[G]])(implicit val o: Origin) extends Expr[G] with MessageImpl[G]

final case class UnresolvedChorBranch[G](branches: Seq[(Expr[G], Statement[G])])(val blame: Blame[SeqBranchFailure])(implicit val o: Origin) extends Statement[G] with ControlContainerStatement[G] with UnresolvedChorBranchImpl[G]
final case class UnresolvedChorLoop[G](cond: Expr[G], contract: LoopContract[G], body: Statement[G])(val blame: Blame[SeqLoopFailure])(implicit val o: Origin) extends Statement[G] with ControlContainerStatement[G] with UnresolvedChorLoopImpl[G]

final case class ChorStatement[G](endpoint: Option[Ref[G, Endpoint[G]]], inner: Statement[G])(val blame: Blame[ChorStatementFailure])(implicit val o: Origin) extends Statement[G] with ChorStatementImpl[G]

@family sealed trait ChorGuard[G] extends NodeFamily[G] with ChorGuardImpl[G]
final case class EndpointGuard[G](endpoint: Ref[G, Endpoint[G]], condition: Expr[G])(implicit val o: Origin) extends ChorGuard[G] with EndpointGuardImpl[G]
final case class UnpointedGuard[G](condition: Expr[G])(implicit val o: Origin) extends ChorGuard[G] with UnpointedGuardImpl[G]

final case class ChorBranch[G](guards: Seq[ChorGuard[G]], yes: Statement[G], no:  Option[Statement[G]])(val blame: Blame[SeqBranchFailure])(implicit val o: Origin) extends Statement[G] with ControlContainerStatement[G] with ChorBranchImpl[G]
final case class ChorLoop[G](guards: Seq[ChorGuard[G]], contract: LoopContract[G], body: Statement[G])(val blame: Blame[SeqLoopFailure])(implicit val o: Origin) extends Statement[G] with ControlContainerStatement[G] with ChorLoopImpl[G]

final case class VeyMontAssignExpression[G](endpoint: Ref[G, Endpoint[G]], assign: Statement[G])(implicit val o: Origin) extends Statement[G] with ControlContainerStatement[G] with VeyMontAssignExpressionImpl[G]
final case class CommunicateX[G](receiver: Ref[G, Endpoint[G]], sender: Ref[G, Endpoint[G]], chanType: Type[G], assign: Statement[G])(implicit val o: Origin) extends Statement[G] with ControlContainerStatement[G] with CommunicateXImpl[G]

sealed trait SilverExpr[G] extends Expr[G] with SilverExprImpl[G]
final case class SilverDeref[G](obj: Expr[G], field: Ref[G, SilverField[G]])(val blame: Blame[InsufficientPermission])(implicit val o: Origin) extends SilverExpr[G] with HeapDeref[G] with SilverDerefImpl[G]
final case class SilverIntToRat[G](perm: Expr[G])(implicit val o: Origin) extends SilverExpr[G] with SilverIntToRatImpl[G]
final case class SilverNull[G]()(implicit val o: Origin) extends SilverExpr[G] with SilverNullImpl[G]
final case class SilverSeqSize[G](seq: Expr[G])(implicit val o: Origin) extends SilverExpr[G] with SilverSeqSizeImpl[G]
final case class SilverSetSize[G](set: Expr[G])(implicit val o: Origin) extends SilverExpr[G] with SilverSetSizeImpl[G]
final case class SilverBagSize[G](bag: Expr[G])(implicit val o: Origin) extends SilverExpr[G] with SilverBagSizeImpl[G]
final case class SilverMapSize[G](map: Expr[G])(implicit val o: Origin) extends SilverExpr[G] with SilverMapSizeImpl[G]

final case class SilverCurFieldPerm[G](obj: Expr[G], field: Ref[G, SilverField[G]])(implicit val o: Origin) extends SilverExpr[G] with SilverCurFieldPermImpl[G]
final case class SilverCurPredPerm[G](ref: Ref[G, Predicate[G]], args: Seq[Expr[G]])(implicit val o: Origin) extends SilverExpr[G] with SilverCurPredPermImpl[G]

final case class SilverPartialADTFunctionInvocation[G](name: String, args: Seq[Expr[G]], partialTypeArgs: Seq[(Ref[G, Variable[G]], Type[G])])(implicit val o: Origin) extends SilverExpr[G] with SilverPartialADTFunctionInvocationImpl[G] {
  var ref: Option[(AxiomaticDataType[G], ADTFunction[G])] = None
}
final case class SilverUntypedNonemptyLiteralMap[G](values: Seq[(Expr[G], Expr[G])])(implicit val o: Origin) extends SilverExpr[G] with SilverUntypedNonemptyLiteralMapImpl[G]

sealed trait SilverStatement[G] extends Statement[G] with SilverStatementImpl[G]
final case class SilverNewRef[G](v: Ref[G, Variable[G]], fields: Seq[Ref[G, SilverField[G]]])(implicit val o: Origin) extends SilverStatement[G] with PurelySequentialStatement[G] with SilverNewRefImpl[G]

sealed trait SilverAssign[G] extends SilverStatement[G] with SilverAssignImpl[G]
final case class SilverFieldAssign[G](obj: Expr[G], field: Ref[G, SilverField[G]], value: Expr[G])(val blame: Blame[AssignFailed])(implicit val o: Origin) extends SilverAssign[G] with ExpressionContainerStatement[G] with SilverFieldAssignImpl[G]
final case class SilverLocalAssign[G](v: Ref[G, Variable[G]], value: Expr[G])(implicit val o: Origin) extends SilverAssign[G] with ExpressionContainerStatement[G] with SilverLocalAssignImpl[G]

sealed trait SilverDeclaration[G] extends GlobalDeclaration[G] with SilverDeclarationImpl[G]
final class SilverField[G](val t: Type[G])(implicit val o: Origin) extends SilverDeclaration[G] with SilverFieldImpl[G]

sealed trait SilverType[G] extends Type[G] with SilverTypeImpl[G]
case class SilverPartialTAxiomatic[G](ref: Ref[G, AxiomaticDataType[G]], partialTypeArgs: Seq[(Ref[G, Variable[G]], Type[G])])(implicit val o: Origin = DiagnosticOrigin) extends SilverType[G] with SilverPartialTAxiomaticImpl[G]