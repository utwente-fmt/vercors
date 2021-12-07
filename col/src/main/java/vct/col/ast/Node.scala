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
import vct.col.ast.temporaryimplpackage.family.catchclause._
import vct.col.ast.temporaryimplpackage.family.contract._
import vct.col.ast.temporaryimplpackage.family.fieldflag._
import vct.col.ast.temporaryimplpackage.family.itervariable._
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

sealed trait Node extends NodeImpl

sealed trait NodeFamily extends Node with NodeFamilyImpl

final case class Program(declarations: Seq[GlobalDeclaration])(val blame: Blame[UnsafeCoercion])(implicit val o: Origin) extends NodeFamily with ProgramImpl

sealed trait Type extends NodeFamily with TypeImpl

final class TNotAValue(var decl: Option[Referrable])(implicit val o: Origin = DiagnosticOrigin) extends Type with TNotAValueImpl
final case class TUnion(types: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends Type with TUnionImpl
final case class TArray(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends Type with TArrayImpl
final case class TPointer(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends Type with TPointerImpl
final case class TType(t: Type)(implicit val o: Origin = DiagnosticOrigin) extends Type with TTypeImpl
final case class TVar(ref: Ref[Variable])(implicit val o: Origin = DiagnosticOrigin) extends Type with TVarImpl

sealed trait CompositeType extends Type with CompositeTypeImpl
sealed trait SizedType extends CompositeType with SizedTypeImpl
final case class TSeq(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends SizedType with TSeqImpl
final case class TSet(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends SizedType with TSetImpl
final case class TBag(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends SizedType with TBagImpl

final case class TOption(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends CompositeType with TOptionImpl
final case class TTuple(elements: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends CompositeType with TTupleImpl
final case class TEither(left: Type, right: Type)(implicit val o: Origin = DiagnosticOrigin) extends CompositeType with TEitherImpl
final case class TMatrix(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends CompositeType with TMatrixImpl
final case class TMap(key: Type, value: Type)(implicit val o: Origin = DiagnosticOrigin) extends CompositeType with TMapImpl

sealed trait PrimitiveType extends Type with PrimitiveTypeImpl
final case class TAny()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TAnyImpl
final case class TNothing()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TNothingImpl
final case class TVoid()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TVoidImpl
final case class TNull()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TNullImpl
final case class TBool()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TBoolImpl
final case class TResource()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TResourceImpl
final case class TChar()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TCharImpl
final case class TString()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TStringImpl
final case class TRef()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TRefImpl
final case class TProcess()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TProcessImpl

sealed trait NumericType extends PrimitiveType with NumericTypeImpl
final case class TInt()(implicit val o: Origin = DiagnosticOrigin) extends NumericType with TIntImpl
final case class TBoundedInt(gte: BigInt, lt: BigInt)(implicit val o: Origin = DiagnosticOrigin) extends NumericType with TBoundedIntImpl
final case class TFloat()(implicit val o: Origin = DiagnosticOrigin) extends NumericType with TFloatImpl
final case class TRational()(implicit val o: Origin = DiagnosticOrigin) extends NumericType with TRationalImpl
final case class TFraction()(implicit val o: Origin = DiagnosticOrigin) extends NumericType with TFractionImpl
final case class TZFraction()(implicit val o: Origin = DiagnosticOrigin) extends NumericType with TZFractionImpl

sealed trait DeclaredType extends Type with DeclaredTypeImpl
final case class TModel(model: Ref[Model])(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType with TModelImpl
final case class TClass(cls: Ref[Class])(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType with TClassImpl
final case class TAxiomatic(adt: Ref[AxiomaticDataType], args: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType with TAxiomaticImpl

sealed trait ParRegion extends NodeFamily with ParRegionImpl
final case class ParParallel(regions: Seq[ParRegion])(val blame: Blame[ParPreconditionFailed])(implicit val o: Origin) extends ParRegion with ParParallelImpl
final case class ParSequential(regions: Seq[ParRegion])(val blame: Blame[ParPreconditionFailed])(implicit val o: Origin) extends ParRegion with ParSequentialImpl
final case class ParBlock(decl: ParBlockDecl, iters: Seq[IterVariable], requires: Expr, ensures: Expr, content: Statement)(val blame: Blame[ParBlockFailure])(implicit val o: Origin) extends ParRegion with ParBlockImpl

sealed trait LoopContract extends NodeFamily with LoopContractImpl
final case class LoopInvariant(invariant: Expr)(implicit val o: Origin) extends LoopContract with LoopInvariantImpl
final case class IterationContract(requires: Expr, ensures: Expr)(implicit val o: Origin) extends LoopContract with IterationContractImpl

final case class CatchClause(decl: Variable, body: Statement)(implicit val o: Origin) extends NodeFamily with CatchClauseImpl

final case class IterVariable(variable: Variable, from: Expr, to: Expr)(implicit val o: Origin) extends NodeFamily with IterVariableImpl

sealed trait Statement extends NodeFamily with StatementImpl

sealed trait NonExecutableStatement extends Statement with NonExecutableStatementImpl
final case class LocalDecl(local: Variable)(implicit val o: Origin) extends NonExecutableStatement with LocalDeclImpl
final case class SpecIgnoreStart()(implicit val o: Origin) extends NonExecutableStatement with SpecIgnoreStartImpl
final case class SpecIgnoreEnd()(implicit val o: Origin) extends NonExecutableStatement with SpecIgnoreEndImpl

sealed trait NormallyCompletingStatement extends Statement with NormallyCompletingStatementImpl
final case class Assign(target: Expr, value: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with AssignImpl
final case class Send(resource: Expr, label: Ref[LabelDecl], offset: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with SendImpl
final case class Recv(resource: Expr, label: Ref[LabelDecl], offset: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with RecvImpl
sealed trait SwitchCase extends NormallyCompletingStatement with SwitchCaseImpl
final case class DefaultCase()(implicit val o: Origin) extends SwitchCase with DefaultCaseImpl
final case class Case(pattern: Expr)(implicit val o: Origin) extends SwitchCase with CaseImpl
final case class Label(decl: LabelDecl, stat: Statement)(implicit val o: Origin) extends NormallyCompletingStatement with LabelImpl
final case class Goto(lbl: Ref[LabelDecl])(implicit val o: Origin) extends NormallyCompletingStatement with GotoImpl
final case class Exhale(res: Expr)(val blame: Blame[ExhaleFailed])(implicit val o: Origin) extends NormallyCompletingStatement with ExhaleImpl
final case class Assert(res: Expr)(val blame: Blame[AssertFailed])(implicit val o: Origin) extends NormallyCompletingStatement with AssertImpl
final case class Refute(assn: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with RefuteImpl
final case class Inhale(res: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with InhaleImpl
final case class Assume(assn: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with AssumeImpl
final case class Wait(obj: Expr)(val blame: Blame[UnlockFailure])(implicit val o: Origin) extends NormallyCompletingStatement with WaitImpl
final case class Notify(obj: Expr)(val blame: Blame[NotifyFailed])(implicit val o: Origin) extends NormallyCompletingStatement with NotifyImpl
final case class Fork(obj: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with ForkImpl
final case class Join(obj: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with JoinImpl
final case class Lock(obj: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with LockImpl
final case class Unlock(obj: Expr)(val blame: Blame[UnlockFailure])(implicit val o: Origin) extends NormallyCompletingStatement with UnlockImpl
final case class Commit(obj: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with CommitImpl
final case class Fold(res: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with FoldImpl
final case class Unfold(res: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with UnfoldImpl
final case class WandQed(res: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with WandQedImpl
final case class WandApply(res: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with WandApplyImpl
final case class WandUse(res: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with WandUseImpl
final case class Havoc(loc: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with HavocImpl

sealed trait ExceptionalStatement extends Statement with ExceptionalStatementImpl
final case class Eval(expr: Expr)(implicit val o: Origin) extends ExceptionalStatement with EvalImpl
final case class Return(result: Expr)(implicit val o: Origin) extends ExceptionalStatement with ReturnImpl
final case class Throw(obj: Expr)(val blame: Blame[ThrowNull])(implicit val o: Origin) extends ExceptionalStatement with ThrowImpl
final case class Break(label: Option[Ref[LabelDecl]])(implicit val o: Origin) extends ExceptionalStatement with BreakImpl
final case class Continue(label: Option[Ref[LabelDecl]])(implicit val o: Origin) extends ExceptionalStatement with ContinueImpl

sealed trait CompositeStatement extends Statement with CompositeStatementImpl
final case class Block(statements: Seq[Statement])(implicit val o: Origin) extends Statement with BlockImpl
final case class Scope(locals: Seq[Variable], body: Statement)(implicit val o: Origin) extends Statement with ScopeImpl
final case class Branch(branches: Seq[(Expr, Statement)])(implicit val o: Origin) extends Statement with BranchImpl
final case class Switch(expr: Expr, body: Statement)(implicit val o: Origin) extends Statement with SwitchImpl
final case class Loop(init: Statement, cond: Expr, update: Statement, contract: LoopContract, body: Statement)(implicit val o: Origin) extends Statement with LoopImpl
final case class TryCatchFinally(body: Statement, after: Statement, catches: Seq[CatchClause])(implicit val o: Origin) extends Statement with TryCatchFinallyImpl
final case class Synchronized(obj: Expr, body: Statement)(val blame: Blame[UnlockFailure])(implicit val o: Origin) extends Statement with SynchronizedImpl
final case class ParInvariant(decl: ParInvariantDecl, inv: Expr, content: Statement)(val blame: Blame[ParInvariantNotEstablished])(implicit val o: Origin) extends Statement with ParInvariantImpl
final case class ParAtomic(inv: Seq[Ref[ParInvariantDecl]], content: Statement)(implicit val o: Origin) extends Statement with ParAtomicImpl
final case class ParBarrier(block: Ref[ParBlockDecl], invs: Seq[Ref[ParInvariantDecl]], requires: Expr, ensures: Expr, content: Statement)(val blame: Blame[ParBarrierFailed])(implicit val o: Origin) extends Statement with ParBarrierImpl
final case class ParStatement(impl: ParRegion)(val blame: Blame[Nothing])(implicit val o: Origin) extends Statement with ParStatementImpl
final case class VecBlock(iters: Seq[IterVariable], requires: Expr, ensures: Expr, content: Statement)(implicit val o: Origin) extends Statement with VecBlockImpl
final case class WandCreate(statements: Seq[Statement])(implicit val o: Origin) extends Statement with WandCreateImpl
final case class ModelDo(model: Expr, perm: Expr, after: Expr, action: Expr, impl: Statement)(implicit val o: Origin) extends Statement with ModelDoImpl

sealed abstract class Declaration extends Node with DeclarationImpl {
  var debugRewriteState: DebugRewriteState = NotProcessed
}

sealed abstract class GlobalDeclaration extends Declaration with GlobalDeclarationImpl
final class SimplificationRule(val axiom: Expr)(implicit val o: Origin) extends GlobalDeclaration with SimplificationRuleImpl
final class AxiomaticDataType(val decls: Seq[ADTDeclaration], val typeArgs: Seq[Variable])(implicit val o: Origin) extends GlobalDeclaration with AxiomaticDataTypeImpl
final class Class(val declarations: Seq[ClassDeclaration], val supports: Seq[Ref[Class]], val intrinsicLockInvariant: Expr)(implicit val o: Origin) extends GlobalDeclaration with ClassImpl
final class Model(val declarations: Seq[ModelDeclaration])(implicit val o: Origin) extends GlobalDeclaration with Declarator with ModelImpl
final class Function(val returnType: Type, val args: Seq[Variable], val typeArgs: Seq[Variable],
               val body: Option[Expr], val contract: ApplicableContract, val inline: Boolean = false)
              (val blame: Blame[PostconditionFailed])(implicit val o: Origin)
  extends GlobalDeclaration with AbstractFunction with FunctionImpl
final class Procedure(val returnType: Type,
                val args: Seq[Variable], val outArgs: Seq[Variable], val typeArgs: Seq[Variable],
                val body: Option[Statement],
                val contract: ApplicableContract,
                val inline: Boolean = false, val pure: Boolean = false)
               (val blame: Blame[PostconditionFailed])(implicit val o: Origin)
  extends GlobalDeclaration with AbstractMethod with ProcedureImpl
final class Predicate(val args: Seq[Variable], val body: Option[Expr],
                val threadLocal: Boolean = false, val inline: Boolean = false)(implicit val o: Origin)
  extends GlobalDeclaration with AbstractPredicate with PredicateImpl

sealed abstract class ClassDeclaration extends Declaration with ClassDeclarationImpl
final class InstanceFunction(val returnType: Type, val args: Seq[Variable], val typeArgs: Seq[Variable],
                       val body: Option[Expr], val contract: ApplicableContract, val inline: Boolean)
                      (val blame: Blame[PostconditionFailed])(implicit val o: Origin)
  extends ClassDeclaration with AbstractFunction with InstanceFunctionImpl
final class InstanceMethod(val returnType: Type,
                     val args: Seq[Variable], val outArgs: Seq[Variable], val typeArgs: Seq[Variable],
                     val body: Option[Statement],
                     val contract: ApplicableContract,
                     val inline: Boolean = false, val pure: Boolean = false)
                    (val blame: Blame[PostconditionFailed])(implicit val o: Origin)
  extends ClassDeclaration with AbstractMethod with InstanceMethodImpl
final class InstancePredicate(val args: Seq[Variable], val body: Option[Expr],
                        val threadLocal: Boolean = false, val inline: Boolean = false)(implicit val o: Origin)
  extends ClassDeclaration with AbstractPredicate with InstancePredicateImpl
final class InstanceField(val t: Type, val flags: Set[FieldFlag])(implicit val o: Origin) extends ClassDeclaration with Field with InstanceFieldImpl

sealed trait ModelDeclaration extends Declaration with ModelDeclarationImpl
final class ModelField(val t: Type)(implicit val o: Origin) extends ModelDeclaration with Field with ModelFieldImpl
final class ModelProcess(val args: Seq[Variable], val impl: Expr,
                   val requires: Expr, val ensures: Expr,
                   val modifies: Seq[Ref[ModelField]], val accessible: Seq[Ref[ModelField]])
                  (val blame: Blame[PostconditionFailed])
                  (implicit val o: Origin) extends ModelDeclaration with Applicable with ModelProcessImpl
final class ModelAction(val args: Seq[Variable],
                  val requires: Expr, val ensures: Expr,
                  val modifies: Seq[Ref[ModelField]], val accessible: Seq[Ref[ModelField]])
                 (implicit val o: Origin) extends ModelDeclaration with Applicable with ModelActionImpl

sealed trait ADTDeclaration extends Declaration with ADTDeclarationImpl
final class ADTAxiom(val axiom: Expr)(implicit val o: Origin) extends ADTDeclaration with ADTAxiomImpl
final class ADTFunction(val args: Seq[Variable], val returnType: Type)(implicit val o: Origin) extends Applicable with ADTDeclaration with ADTFunctionImpl

final class Variable(val t: Type)(implicit val o: Origin) extends Declaration with VariableImpl
final class LabelDecl()(implicit val o: Origin) extends Declaration with LabelDeclImpl
final class ParBlockDecl()(implicit val o: Origin) extends Declaration with ParBlockDeclImpl
final class ParInvariantDecl()(implicit val o: Origin) extends Declaration with ParInvariantDeclImpl

sealed trait Applicable extends Declaration with ApplicableImpl
sealed trait InlineableApplicable extends Applicable with InlineableApplicableImpl
sealed trait AbstractPredicate extends InlineableApplicable with AbstractPredicateImpl
sealed trait ContractApplicable extends InlineableApplicable with ContractApplicableImpl
sealed trait AbstractFunction extends ContractApplicable with AbstractFunctionImpl
sealed trait AbstractMethod extends ContractApplicable with AbstractMethodImpl
sealed trait Field extends FieldImpl

final case class SignalsClause(binding: Variable, assn: Expr)(implicit val o: Origin) extends NodeFamily with SignalsClauseImpl

final case class ApplicableContract(requires: Expr, ensures: Expr, contextEverywhere: Expr,
                              signals: Seq[SignalsClause], givenArgs: Seq[Variable], yieldsArgs: Seq[Variable])
                             (implicit val o: Origin) extends NodeFamily with ApplicableContractImpl

sealed trait FieldFlag extends NodeFamily with FieldFlagImpl
final class Final()(implicit val o: Origin) extends FieldFlag with FinalImpl

sealed trait Expr extends NodeFamily with ExprImpl

sealed abstract class Constant[T] extends Expr with ConstantImpl[T]
final case class IntegerValue(value: BigInt)(implicit val o: Origin) extends Constant[BigInt] with Expr with IntegerValueImpl
final case class BooleanValue(value: Boolean)(implicit val o: Origin) extends Constant[Boolean] with BooleanValueImpl
final case class LiteralSeq(element: Type, values: Seq[Expr])(implicit val o: Origin) extends Expr with LiteralSeqImpl
final case class LiteralSet(element: Type, values: Seq[Expr])(implicit val o: Origin) extends Expr with LiteralSetImpl
final case class LiteralBag(element: Type, values: Seq[Expr])(implicit val o: Origin) extends Expr with LiteralBagImpl
final case class LiteralTuple(ts: Seq[Type], values: Seq[Expr])(implicit val o: Origin) extends Expr with LiteralTupleImpl
final case class LiteralMap(k: Type, v: Type, values: Seq[(Expr, Expr)])(implicit val o: Origin) extends Expr with LiteralMapImpl
final case class UntypedLiteralSeq(values: Seq[Expr])(implicit val o: Origin) extends Expr with UntypedLiteralSeqImpl
final case class UntypedLiteralSet(values: Seq[Expr])(implicit val o: Origin) extends Expr with UntypedLiteralSetImpl
final case class UntypedLiteralBag(values: Seq[Expr])(implicit val o: Origin) extends Expr with UntypedLiteralBagImpl
final case class Void()(implicit val o: Origin) extends Expr with VoidImpl
final case class Null()(implicit val o: Origin) extends Expr with NullImpl
final case class NoPerm()(implicit val o: Origin) extends Expr with NoPermImpl
final case class WritePerm()(implicit val o: Origin) extends Expr with WritePermImpl
final case class OptSome(e: Expr)(implicit val o: Origin) extends Expr with OptSomeImpl
final case class OptNone()(implicit val o: Origin) extends Expr with OptNoneImpl
final case class Range(from: Expr, to: Expr)(implicit val o: Origin) extends Expr with RangeImpl
final case class EitherLeft(e: Expr)(implicit val o: Origin) extends Expr with EitherLeftImpl
final case class EitherRight(e: Expr)(implicit val o: Origin) extends Expr with EitherRightImpl
final case class MapCons(map: Expr, k: Expr, v: Expr)(implicit val o: Origin) extends Expr with MapConsImpl

final case class AmbiguousThis()(implicit val o: Origin) extends Expr with AmbiguousThisImpl {
  var ref: Option[ThisTarget] = None
}

final case class ThisObject(cls: Ref[Class])(implicit val o: Origin) extends Expr with ThisObjectImpl
final case class ThisModel(cls: Ref[Model])(implicit val o: Origin) extends Expr with ThisModelImpl

final case class AmbiguousResult()(implicit val o: Origin) extends Expr with AmbiguousResultImpl {
  var ref: Option[ResultTarget] = None
}

final case class Result(applicable: Ref[ContractApplicable])(implicit val o: Origin) extends Expr with ResultImpl
final case class CurrentThreadId()(implicit val o: Origin) extends Expr with CurrentThreadIdImpl
final case class Any()(implicit val o: Origin) extends Expr with AnyImpl
final case class ReadPerm()(implicit val o: Origin) extends Expr with ReadPermImpl
final case class Values(arr: Expr, from: Expr, to: Expr)(val blame: Blame[ArrayValuesError])(implicit val o: Origin) extends Expr with ValuesImpl
final case class MapEq(left: Expr, right: Expr)(implicit val o: Origin) extends Expr with MapEqImpl
final case class MapDisjoint(left: Expr, right: Expr)(implicit val o: Origin) extends Expr with MapDisjointImpl
sealed trait MapOp extends Expr with MapOpImpl
final case class MapKeySet(map: Expr)(implicit val o: Origin) extends MapOp with MapKeySetImpl
final case class MapValueSet(map: Expr)(implicit val o: Origin) extends MapOp with MapValueSetImpl
final case class MapItemSet(map: Expr)(implicit val o: Origin) extends MapOp with MapItemSetImpl
final case class MapSize(map: Expr)(implicit val o: Origin) extends MapOp with MapSizeImpl
final case class MapRemove(map: Expr, k: Expr)(implicit val o: Origin) extends Expr with MapRemoveImpl

sealed trait Binder extends Expr with Declarator with BinderImpl
final case class Forall(bindings: Seq[Variable], triggers: Seq[Seq[Expr]], body: Expr)(implicit val o: Origin) extends Binder with ForallImpl
final case class Starall(bindings: Seq[Variable], triggers: Seq[Seq[Expr]], body: Expr)(implicit val o: Origin) extends Binder with StarallImpl
final case class Exists(bindings: Seq[Variable], triggers: Seq[Seq[Expr]], body: Expr)(implicit val o: Origin) extends Binder with ExistsImpl
final case class Sum(bindings: Seq[Variable], condition: Expr, main: Expr)(implicit val o: Origin) extends Binder with SumImpl
final case class Product(bindings: Seq[Variable], condition: Expr, main: Expr)(implicit val o: Origin) extends Binder with ProductImpl
final case class Let(binding: Variable, value: Expr, main: Expr)(implicit val o: Origin) extends Binder with LetImpl
final case class InlinePattern(inner: Expr)(implicit val o: Origin) extends Expr with InlinePatternImpl

final case class Local(ref: Ref[Variable])(implicit val o: Origin) extends Expr with LocalImpl
sealed trait HeapDeref extends HeapDerefImpl
final case class Deref(obj: Expr, ref: Ref[InstanceField])(val blame: Blame[InsufficientPermission])(implicit val o: Origin) extends Expr with HeapDeref with DerefImpl
final case class ModelDeref(obj: Expr, ref: Ref[ModelField])(val blame: Blame[ModelInsufficientPermission])(implicit val o: Origin) extends Expr with ModelDerefImpl
final case class DerefPointer(pointer: Expr)(val blame: Blame[PointerDerefError])(implicit val o: Origin) extends Expr with DerefPointerImpl
final case class PointerAdd(pointer: Expr, offset: Expr)(val blame: Blame[PointerAddError])(implicit val o: Origin) extends Expr with PointerAddImpl
final case class AddrOf(e: Expr)(implicit val o: Origin) extends Expr with AddrOfImpl

sealed trait Apply extends Expr with ApplyImpl
final case class ADTFunctionInvocation(typeArgs: Option[(Ref[AxiomaticDataType], Seq[Type])], ref: Ref[ADTFunction], args: Seq[Expr])(implicit val o: Origin) extends Apply with ADTFunctionInvocationImpl

sealed trait ApplyInlineable extends Apply with ApplyInlineableImpl

sealed trait ApplyAnyPredicate extends ApplyInlineable with ApplyAnyPredicateImpl
final case class PredicateApply(ref: Ref[Predicate], args: Seq[Expr])(implicit val o: Origin) extends ApplyAnyPredicate with PredicateApplyImpl
final case class InstancePredicateApply(obj: Expr, ref: Ref[InstancePredicate], args: Seq[Expr])(implicit val o: Origin) extends ApplyAnyPredicate with InstancePredicateApplyImpl

sealed trait Invocation extends ApplyInlineable with InvocationImpl

sealed trait AnyMethodInvocation extends Invocation with AnyMethodInvocationImpl
final case class ProcedureInvocation(ref: Ref[Procedure], args: Seq[Expr], outArgs: Seq[Ref[Variable]], typeArgs: Seq[Type])(val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends AnyMethodInvocation with ProcedureInvocationImpl
final case class MethodInvocation(obj: Expr, ref: Ref[InstanceMethod], args: Seq[Expr], outArgs: Seq[Ref[Variable]], typeArgs: Seq[Type])(val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends AnyMethodInvocation with MethodInvocationImpl

sealed trait AnyFunctionInvocation extends Invocation with AnyFunctionInvocationImpl
final case class FunctionInvocation(ref: Ref[Function], args: Seq[Expr], typeArgs: Seq[Type])(val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends AnyFunctionInvocation with FunctionInvocationImpl
final case class InstanceFunctionInvocation(obj: Expr, ref: Ref[InstanceFunction], args: Seq[Expr], typeArgs: Seq[Type])(val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends AnyFunctionInvocation with InstanceFunctionInvocationImpl

sealed trait UnExpr extends Expr with UnExprImpl

final case class UMinus(arg: Expr)(implicit val o: Origin) extends UnExpr with UMinusImpl
final case class BitNot(arg: Expr)(implicit val o: Origin) extends UnExpr with BitNotImpl
final case class Not(arg: Expr)(implicit val o: Origin) extends UnExpr with NotImpl

sealed trait BinExpr extends Expr with BinExprImpl
sealed trait NumericBinExpr extends BinExpr with NumericBinExprImpl

sealed trait DividingExpr extends Expr with DividingExprImpl

final case class AmbiguousMult(left: Expr, right: Expr)(implicit val o: Origin) extends Expr with AmbiguousMultImpl
final case class AmbiguousPlus(left: Expr, right: Expr)(val blame: Blame[FrontendPlusError])(implicit val o: Origin) extends Expr with AmbiguousPlusImpl
final case class AmbiguousOr(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with AmbiguousOrImpl

sealed trait BitOp extends BinExpr with BitOpImpl
final case class AmbiguousComputationalOr(left: Expr, right: Expr)(implicit val o: Origin) extends BitOp with AmbiguousComputationalOrImpl
final case class AmbiguousComputationalXor(left: Expr, right: Expr)(implicit val o: Origin) extends BitOp with AmbiguousComputationalXorImpl
final case class AmbiguousComputationalAnd(left: Expr, right: Expr)(implicit val o: Origin) extends BitOp with AmbiguousComputationalAndImpl

final case class ComputationalOr(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with ComputationalOrImpl
final case class ComputationalXor(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with ComputationalXorImpl
final case class ComputationalAnd(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with ComputationalAndImpl

final case class Exp(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr with ExpImpl
final case class Plus(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr with PlusImpl
final case class Minus(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr with MinusImpl
final case class Mult(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr with MultImpl
final case class Div(left: Expr, right: Expr)(val blame: Blame[DivByZero])(implicit val o: Origin) extends BinExpr with DividingExpr with DivImpl
final case class FloorDiv(left: Expr, right: Expr)(val blame: Blame[DivByZero])(implicit val o: Origin) extends BinExpr with DividingExpr with FloorDivImpl
final case class Mod(left: Expr, right: Expr)(val blame: Blame[DivByZero])(implicit val o: Origin) extends NumericBinExpr with DividingExpr with ModImpl

final case class BitAnd(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with BitAndImpl
final case class BitOr(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with BitOrImpl
final case class BitXor(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with BitXorImpl
final case class BitShl(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with BitShlImpl
final case class BitShr(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with BitShrImpl
final case class BitUShr(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with BitUShrImpl

final case class And(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with AndImpl
final case class Or(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with OrImpl
final case class Implies(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with ImpliesImpl
final case class Star(left: Expr, right: Expr)(implicit val o: Origin) extends Expr with StarImpl
final case class Wand(left: Expr, right: Expr)(implicit val o: Origin) extends Expr with WandImpl
final case class Scale(scale: Expr, res: Expr)(implicit val o: Origin) extends Expr with ScaleImpl

final case class Unfolding(pred: Expr, body: Expr)(implicit val o: Origin) extends Expr with UnfoldingImpl

final case class Perm(loc: Expr, perm: Expr)(implicit val o: Origin) extends Expr with PermImpl
final case class HPerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends Expr with HPermImpl
final case class APerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends Expr with APermImpl
final case class PointsTo(loc: Expr, perm: Expr, value: Expr)(implicit val o: Origin) extends Expr with PointsToImpl

final case class CurPerm(loc: Expr)(implicit val o: Origin) extends Expr with CurPermImpl

final case class ValidArray(arr: Expr, len: Expr)(implicit val o: Origin) extends Expr with ValidArrayImpl
final case class ValidMatrix(mat: Expr, w: Expr, h: Expr)(implicit val o: Origin) extends Expr with ValidMatrixImpl

final case class PermPointer(p: Expr, len: Expr, perm: Expr)(implicit val o: Origin) extends Expr with PermPointerImpl
final case class PermPointerIndex(p: Expr, idx: Expr, perm: Expr)(implicit val o: Origin) extends Expr with PermPointerIndexImpl

sealed trait Comparison extends BinExpr with ComparisonImpl
final case class Eq(left: Expr, right: Expr)(implicit val o: Origin) extends Comparison with EqImpl
final case class Neq(left: Expr, right: Expr)(implicit val o: Origin) extends Comparison with NeqImpl

sealed trait OrderOp extends Comparison with OrderOpImpl
final case class Greater(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp with GreaterImpl
final case class Less(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp with LessImpl
final case class GreaterEq(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp with GreaterEqImpl
final case class LessEq(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp with LessEqImpl

final case class Select(condition: Expr, whenTrue: Expr, whenFalse: Expr)(implicit val o: Origin) extends Expr with SelectImpl
final case class NewObject(cls: Ref[Class])(implicit val o: Origin) extends Expr with NewObjectImpl
final case class NewArray(element: Type, dims: Seq[Expr], moreDims: Int)(implicit val o: Origin) extends Expr with NewArrayImpl
final case class Old(expr: Expr, at: Option[Ref[LabelDecl]])(val blame: Blame[LabelNotReached])(implicit val o: Origin) extends Expr with OldImpl
final case class AmbiguousSubscript(collection: Expr, index: Expr)(val blame: Blame[FrontendSubscriptError])(implicit val o: Origin) extends Expr with AmbiguousSubscriptImpl
final case class SeqSubscript(seq: Expr, index: Expr)(val blame: Blame[SeqBoundFailure])(implicit val o: Origin) extends Expr with SeqSubscriptImpl
final case class ArraySubscript(arr: Expr, index: Expr)(val blame: Blame[ArraySubscriptError])(implicit val o: Origin) extends Expr with ArraySubscriptImpl
final case class PointerSubscript(pointer: Expr, index: Expr)(val blame: Blame[PointerSubscriptError])(implicit val o: Origin) extends Expr with PointerSubscriptImpl
final case class Length(arr: Expr)(val blame: Blame[ArrayNull])(implicit val o: Origin) extends Expr with LengthImpl
final case class Size(obj: Expr)(implicit val o: Origin) extends Expr with SizeImpl
final case class Cons(x: Expr, xs: Expr)(implicit val o: Origin) extends Expr with ConsImpl

final case class Head(xs: Expr)(val blame: Blame[SeqBoundFailure])(implicit val o: Origin) extends Expr with HeadImpl
final case class Tail(xs: Expr)(implicit val o: Origin) extends Expr with TailImpl
final case class Drop(xs: Expr, count: Expr)(implicit val o: Origin) extends Expr with DropImpl
final case class Take(xs: Expr, count: Expr)(implicit val o: Origin) extends Expr with TakeImpl
final case class Slice(xs: Expr, from: Expr, to: Expr)(implicit val o: Origin) extends Expr with SliceImpl
final case class SeqUpdate(xs: Expr, i: Expr, x: Expr)(implicit val o: Origin) extends Expr with SeqUpdateImpl
final case class Concat(xs: Expr, ys: Expr)(implicit val o: Origin) extends Expr with ConcatImpl
final case class RemoveAt(xs: Expr, i: Expr)(implicit val o: Origin) extends Expr with RemoveAtImpl
final case class Empty(obj: Expr)(implicit val o: Origin) extends Expr with EmptyImpl

final case class AmbiguousMember(x: Expr, xs: Expr)(implicit val o: Origin) extends Expr with AmbiguousMemberImpl
final case class SetMember(x: Expr, xs: Expr)(implicit val o: Origin) extends Expr with SetMemberImpl
final case class SeqMember(x: Expr, xs: Expr)(implicit val o: Origin) extends Expr with SeqMemberImpl
final case class MapMember(x: Expr, xs: Expr)(implicit val o: Origin) extends Expr with MapMemberImpl
final case class BagMemberCount(x: Expr, xs: Expr)(implicit val o: Origin) extends Expr with BagMemberCountImpl

sealed trait SetComparison extends Comparison with SetComparisonImpl
final case class SubSet(left: Expr, right: Expr)(implicit val o: Origin) extends SetComparison with SubSetImpl
final case class SubSetEq(left: Expr, right: Expr)(implicit val o: Origin) extends SetComparison with SubSetEqImpl
final case class Permutation(xs: Expr, ys: Expr)(implicit val o: Origin) extends Expr with PermutationImpl
final case class OptGet(opt: Expr)(val blame: Blame[OptionNone])(implicit val o: Origin) extends Expr with OptGetImpl
final case class OptGetOrElse(opt: Expr, alt: Expr)(implicit val o: Origin) extends Expr with OptGetOrElseImpl
final case class MapGet(map: Expr, k: Expr)(val blame: Blame[MapKeyError])(implicit val o: Origin) extends Expr with MapGetImpl
final case class TupGet(tup: Expr, index: Int)(implicit val o: Origin) extends Expr with TupGetImpl

sealed trait EitherOp extends Expr with EitherOpImpl
final case class GetLeft(either: Expr)(val blame: Blame[NotLeft])(implicit val o: Origin) extends EitherOp with GetLeftImpl
final case class GetRight(either: Expr)(val blame: Blame[NotRight])(implicit val o: Origin) extends EitherOp with GetRightImpl
final case class IsLeft(either: Expr)(implicit val o: Origin) extends EitherOp with Expr with IsLeftImpl
final case class IsRight(either: Expr)(implicit val o: Origin) extends EitherOp with Expr with IsRightImpl

final case class VectorSum(indices: Expr, vec: Expr)(implicit val o: Origin) extends Expr with VectorSumImpl
final case class VectorCompare(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with VectorCompareImpl
final case class VectorRepeat(e: Expr)(implicit val o: Origin) extends Expr with VectorRepeatImpl
final case class MatrixSum(indices: Expr, mat: Expr)(implicit val o: Origin) extends Expr with MatrixSumImpl
final case class MatrixCompare(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with MatrixCompareImpl
final case class MatrixRepeat(e: Expr)(implicit val o: Origin) extends Expr with MatrixRepeatImpl

final case class TypeValue(value: Type)(implicit val o: Origin) extends Expr with TypeValueImpl
final case class TypeOf(expr: Expr)(implicit val o: Origin) extends Expr with TypeOfImpl
final case class InstanceOf(value: Expr, typeValue: Expr)(implicit val o: Origin) extends Expr with InstanceOfImpl
final case class Cast(value: Expr, typeValue: Expr)(implicit val o: Origin) extends Expr with CastImpl

sealed trait TypeComparison extends Comparison with TypeComparisonImpl
final case class SubType(left: Expr, right: Expr)(implicit val o: Origin) extends TypeComparison with SubTypeImpl
final case class SuperType(left: Expr, right: Expr)(implicit val o: Origin) extends TypeComparison with SuperTypeImpl

sealed trait AssignExpression extends Expr with AssignExpressionImpl
final case class PreAssignExpression(target: Expr, value: Expr)(implicit val o: Origin) extends AssignExpression with PreAssignExpressionImpl
final case class PostAssignExpression(target: Expr, value: Expr)(implicit val o: Origin) extends AssignExpression with PostAssignExpressionImpl

final case class With(pre: Statement, value: Expr)(implicit val o: Origin) extends Expr with WithImpl
final case class Then(value: Expr, post: Statement)(implicit val o: Origin) extends Expr with ThenImpl

final case class Held(obj: Expr)(implicit val o: Origin) extends Expr with HeldImpl
final case class IdleToken(thread: Expr)(implicit val o: Origin) extends Expr with IdleTokenImpl
final case class JoinToken(thread: Expr)(implicit val o: Origin) extends Expr with JoinTokenImpl

final case class EmptyProcess()(implicit val o: Origin) extends Expr with EmptyProcessImpl
final case class ActionApply(action: Ref[ModelAction], args: Seq[Expr])(implicit val o: Origin) extends Expr with ActionApplyImpl
final case class ProcessApply(process: Ref[ModelProcess], args: Seq[Expr])(implicit val o: Origin) extends Expr with ProcessApplyImpl
final case class ProcessSeq(left: Expr, right: Expr)(implicit val o: Origin) extends Expr with ProcessSeqImpl
final case class ProcessChoice(left: Expr, right: Expr)(implicit val o: Origin) extends Expr with ProcessChoiceImpl
final case class ProcessPar(left: Expr, right: Expr)(implicit val o: Origin) extends Expr with ProcessParImpl
final case class ProcessSelect(cond: Expr, whenTrue: Expr, whenFalse: Expr)(implicit val o: Origin) extends Expr with ProcessSelectImpl

final case class ModelNew(ref: Ref[Model])(implicit val o: Origin) extends Expr with ModelNewImpl

final case class ModelState(model: Expr, perm: Expr, state: Expr)(implicit val o: Origin) extends Expr with ModelStateImpl
final case class ModelAbstractState(model: Expr, state: Expr)(implicit val o: Origin) extends Expr with ModelAbstractStateImpl
final case class ModelCreate(model: Expr, init: Expr)(implicit val o: Origin) extends Expr with ModelCreateImpl
final case class ModelDestroy(model: Expr)(implicit val o: Origin) extends Expr with ModelDestroyImpl
final case class ModelSplit(model: Expr, leftPerm: Expr, leftProcess: Expr, rightPerm: Expr, rightProcess: Expr)(implicit val o: Origin) extends Expr with ModelSplitImpl
final case class ModelMerge(model: Expr, leftPerm: Expr, leftProcess: Expr, rightPerm: Expr, rightProcess: Expr)(implicit val o: Origin) extends Expr with ModelMergeImpl
final case class ModelChoose(model: Expr, perm: Expr, totalProcess: Expr, choice: Expr)(implicit val o: Origin) extends Expr with ModelChooseImpl

final case class ModelPerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends Expr with ModelPermImpl
final case class ActionPerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends Expr with ActionPermImpl

sealed trait CDeclarationSpecifier extends NodeFamily with CDeclarationSpecifierImpl

sealed trait CSpecificationModifier extends CDeclarationSpecifier with CSpecificationModifierImpl
final case class CPure()(implicit val o: Origin) extends CSpecificationModifier with CPureImpl
final case class CInline()(implicit val o: Origin) extends CSpecificationModifier with CInlineImpl

sealed trait CStorageClassSpecifier extends CDeclarationSpecifier with CStorageClassSpecifierImpl
final case class CTypedef()(implicit val o: Origin) extends CStorageClassSpecifier with CTypedefImpl
final case class CExtern()(implicit val o: Origin) extends CStorageClassSpecifier with CExternImpl
final case class CStatic()(implicit val o: Origin) extends CStorageClassSpecifier with CStaticImpl

sealed trait CTypeSpecifier extends CDeclarationSpecifier with CTypeSpecifierImpl
final case class CVoid()(implicit val o: Origin) extends CTypeSpecifier with CVoidImpl
final case class CChar()(implicit val o: Origin) extends CTypeSpecifier with CCharImpl
final case class CShort()(implicit val o: Origin) extends CTypeSpecifier with CShortImpl
final case class CInt()(implicit val o: Origin) extends CTypeSpecifier with CIntImpl
final case class CLong()(implicit val o: Origin) extends CTypeSpecifier with CLongImpl
final case class CFloat()(implicit val o: Origin) extends CTypeSpecifier with CFloatImpl
final case class CDouble()(implicit val o: Origin) extends CTypeSpecifier with CDoubleImpl
final case class CSigned()(implicit val o: Origin) extends CTypeSpecifier with CSignedImpl
final case class CUnsigned()(implicit val o: Origin) extends CTypeSpecifier with CUnsignedImpl
final case class CBool()(implicit val o: Origin) extends CTypeSpecifier with CBoolImpl
final case class CTypedefName(name: String)(implicit val o: Origin) extends CTypeSpecifier with CTypedefNameImpl {
  var ref: Option[CTypeNameTarget] = None
}
final case class CSpecificationType(t: Type)(implicit val o: Origin) extends CTypeSpecifier with CSpecificationTypeImpl

final case class CTypeQualifierDeclarationSpecifier(typeQual: CTypeQualifier)(implicit val o: Origin) extends CDeclarationSpecifier with CTypeQualifierDeclarationSpecifierImpl

sealed trait CTypeQualifier extends NodeFamily with CTypeQualifierImpl
final case class CConst()(implicit val o: Origin) extends CTypeQualifier with CConstImpl
final case class CRestrict()(implicit val o: Origin) extends CTypeQualifier with CRestrictImpl
final case class CVolatile()(implicit val o: Origin) extends CTypeQualifier with CVolatileImpl
final case class CAtomic()(implicit val o: Origin) extends CTypeQualifier with CAtomicImpl

sealed trait CFunctionSpecifier extends CDeclarationSpecifier with CFunctionSpecifierImpl
sealed trait CAlignmentSpecifier extends CDeclarationSpecifier with CAlignmentSpecifierImpl

sealed trait CGpgpuKernelSpecifier extends CDeclarationSpecifier with CGpgpuKernelSpecifierImpl
final case class CKernel()(implicit val o: Origin) extends CGpgpuKernelSpecifier with CKernelImpl

final case class CPointer(qualifiers: Seq[CTypeQualifier])(implicit val o: Origin) extends NodeFamily with CPointerImpl

final class CParam(val specifiers: Seq[CDeclarationSpecifier], val declarator: CDeclarator)(implicit val o: Origin) extends Declaration with CParamImpl

sealed trait CDeclarator extends NodeFamily with CDeclaratorImpl
final case class CPointerDeclarator(pointers: Seq[CPointer], inner: CDeclarator)(implicit val o: Origin) extends CDeclarator with CPointerDeclaratorImpl
final case class CArrayDeclarator(qualifiers: Seq[CTypeQualifier], size: Option[Expr], inner: CDeclarator)(implicit val o: Origin) extends CDeclarator with CArrayDeclaratorImpl
final case class CTypedFunctionDeclarator(params: Seq[CParam], varargs: Boolean, inner: CDeclarator)(implicit val o: Origin) extends CDeclarator with CTypedFunctionDeclaratorImpl
final case class CAnonymousFunctionDeclarator(params: Seq[String], inner: CDeclarator)(implicit val o: Origin) extends CDeclarator with CAnonymousFunctionDeclaratorImpl
final case class CName(name: String)(implicit val o: Origin) extends CDeclarator with CNameImpl

final case class CInit(decl: CDeclarator, init: Option[Expr])(implicit val o: Origin) extends NodeFamily with CInitImpl

final class CDeclaration(val contract: ApplicableContract, val kernelInvariant: Expr, val specs: Seq[CDeclarationSpecifier], val inits: Seq[CInit])(implicit val o: Origin) extends Declaration with CDeclarationImpl

sealed trait CAbstractGlobalDeclaration extends GlobalDeclaration with CAbstractGlobalDeclarationImpl

final class CFunctionDefinition(val specs: Seq[CDeclarationSpecifier], val declarator: CDeclarator, val body: Statement)(val blame: Blame[PostconditionFailed])(implicit val o: Origin) extends CAbstractGlobalDeclaration with CFunctionDefinitionImpl

final class CGlobalDeclaration(val decl: CDeclaration)(implicit val o: Origin) extends CAbstractGlobalDeclaration with CGlobalDeclarationImpl

sealed trait CStatement extends Statement with CStatementImpl
final case class CDeclarationStatement(decl: CDeclaration)(implicit val o: Origin) extends CStatement with CDeclarationStatementImpl
final case class CGoto(label: String)(implicit val o: Origin) extends CStatement with CGotoImpl {
  var ref: Option[LabelDecl] = None
}

final case class GpgpuLocalBarrier(requires: Expr, ensures: Expr)(implicit val o: Origin) extends CStatement with GpgpuLocalBarrierImpl
final case class GpgpuGlobalBarrier(requires: Expr, ensures: Expr)(implicit val o: Origin) extends CStatement with GpgpuGlobalBarrierImpl
final case class GpgpuAtomic(impl: Statement, before: Statement, after: Statement)(implicit val o: Origin) extends CStatement with GpgpuAtomicImpl

sealed trait CExpr extends Expr with CExprImpl
final case class CLocal(name: String)(implicit val o: Origin) extends CExpr with CLocalImpl {
  var ref: Option[CNameTarget] = None
}
final case class CInvocation(applicable: Expr, args: Seq[Expr], givenArgs: Seq[(String, Expr)], yields: Seq[(Expr, String)])(implicit val o: Origin) extends CExpr with CInvocationImpl {
  var ref: Option[CInvocationTarget] = None
}
final case class CStructAccess(struct: Expr, field: String)(val blame: Blame[FrontendDerefError])(implicit val o: Origin) extends CExpr with CStructAccessImpl {
  var ref: Option[CDerefTarget] = None
}
final case class CStructDeref(struct: Expr, field: String)(implicit val o: Origin) extends CExpr with CStructDerefImpl
final case class GpgpuCudaKernelInvocation(kernel: String, blocks: Expr, threads: Expr, args: Seq[Expr], givenArgs: Seq[(String, Expr)], yields: Seq[(Expr, String)])(implicit val o: Origin) extends CExpr with GpgpuCudaKernelInvocationImpl {
  var ref: Option[CInvocationTarget] = None
}

sealed trait CType extends Type with CTypeImpl
final case class CPrimitiveType(specifiers: Seq[CDeclarationSpecifier])(implicit val o: Origin = DiagnosticOrigin) extends CType with CPrimitiveTypeImpl

final case class JavaName(names: Seq[String])(implicit val o: Origin) extends NodeFamily with JavaNameImpl {
  var ref: Option[JavaTypeNameTarget] = None
}
final case class JavaImport(isStatic: Boolean, name: JavaName, star: Boolean)(implicit val o: Origin) extends NodeFamily with JavaImportImpl

sealed trait JavaModifier extends NodeFamily with JavaModifierImpl
final case class JavaPublic()(implicit val o: Origin) extends JavaModifier with JavaPublicImpl
final case class JavaProtected()(implicit val o: Origin) extends JavaModifier with JavaProtectedImpl
final case class JavaPrivate()(implicit val o: Origin) extends JavaModifier with JavaPrivateImpl
final case class JavaStatic()(implicit val o: Origin) extends JavaModifier with JavaStaticImpl
final case class JavaAbstract()(implicit val o: Origin) extends JavaModifier with JavaAbstractImpl
final case class JavaFinal()(implicit val o: Origin) extends JavaModifier with JavaFinalImpl
final case class JavaStrictFP()(implicit val o: Origin) extends JavaModifier with JavaStrictFPImpl
final case class JavaNative()(implicit val o: Origin) extends JavaModifier with JavaNativeImpl
final case class JavaSynchronized()(implicit val o: Origin) extends JavaModifier with JavaSynchronizedImpl
final case class JavaTransient()(implicit val o: Origin) extends JavaModifier with JavaTransientImpl
final case class JavaVolatile()(implicit val o: Origin) extends JavaModifier with JavaVolatileImpl

final case class JavaPure()(implicit val o: Origin) extends JavaModifier with JavaPureImpl
final case class JavaInline()(implicit val o: Origin) extends JavaModifier with JavaInlineImpl

sealed trait JavaGlobalDeclaration extends GlobalDeclaration with JavaGlobalDeclarationImpl
final class JavaNamespace(val pkg: Option[JavaName], val imports: Seq[JavaImport], val declarations: Seq[GlobalDeclaration])(implicit val o: Origin) extends JavaGlobalDeclaration with Declarator with JavaNamespaceImpl

sealed abstract class JavaClassOrInterface extends JavaGlobalDeclaration with Declarator with JavaClassOrInterfaceImpl
final class JavaClass(val name: String, val modifiers: Seq[JavaModifier], val typeParams: Seq[Variable], val intrinsicLockInvariant: Expr, val ext: Type, val imp: Seq[Type], val decls: Seq[ClassDeclaration])(implicit val o: Origin) extends JavaClassOrInterface with JavaClassImpl
final class JavaInterface(val name: String, val modifiers: Seq[JavaModifier], val typeParams: Seq[Variable], val ext: Seq[Type], val decls: Seq[ClassDeclaration])(implicit val o: Origin) extends JavaClassOrInterface with JavaInterfaceImpl

sealed trait JavaClassDeclaration extends ClassDeclaration with JavaClassDeclarationImpl
final class JavaSharedInitialization(val isStatic: Boolean, val initialization: Statement)(implicit val o: Origin) extends JavaClassDeclaration with JavaSharedInitializationImpl
final class JavaFields(val modifiers: Seq[JavaModifier], val t: Type, val decls: Seq[(String, Int, Option[Expr])])(implicit val o: Origin) extends JavaClassDeclaration with JavaFieldsImpl
final class JavaConstructor(val modifiers: Seq[JavaModifier], val name: String, val parameters: Seq[Variable], val typeParameters: Seq[Variable], val signals: Seq[JavaName], val body: Statement, val contract: ApplicableContract)(implicit val o: Origin) extends JavaClassDeclaration with JavaConstructorImpl
final class JavaMethod(val modifiers: Seq[JavaModifier], val returnType: Type, val dims: Int, val name: String, val parameters: Seq[Variable], val typeParameters: Seq[Variable], val signals: Seq[JavaName], val body: Option[Statement], val contract: ApplicableContract)(val blame: Blame[PostconditionFailed])(implicit val o: Origin) extends JavaClassDeclaration with JavaMethodImpl

final class JavaLocalDeclaration(val modifiers: Seq[JavaModifier], val t: Type, val decls: Seq[(String, Int, Option[Expr])])(implicit val o: Origin) extends Declaration with JavaLocalDeclarationImpl

sealed trait JavaStatement extends Statement with JavaStatementImpl
final case class JavaLocalDeclarationStatement(decl: JavaLocalDeclaration)(implicit val o: Origin) extends JavaStatement with JavaLocalDeclarationStatementImpl

sealed trait JavaType extends Type with JavaTypeImpl
final case class JavaNamedType(names: Seq[(String, Option[Seq[Type]])])(implicit val o: Origin) extends JavaType with JavaNamedTypeImpl {
  var ref: Option[JavaTypeNameTarget] = None
}
final case class JavaTClass(ref: Ref[JavaClassOrInterface], typeArgs: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends JavaType with JavaTClassImpl

sealed trait JavaExpr extends Expr with JavaExprImpl
final case class JavaLocal(name: String)(val blame: Blame[DerefInsufficientPermission])(implicit val o: Origin) extends JavaExpr with JavaLocalImpl {
  var ref: Option[JavaNameTarget] = None
}
final case class JavaDeref(obj: Expr, field: String)(val blame: Blame[FrontendDerefError])(implicit val o: Origin) extends JavaExpr with JavaDerefImpl {
  var ref: Option[JavaDerefTarget] = None
}
final case class JavaLiteralArray(exprs: Seq[Expr])(implicit val o: Origin) extends JavaExpr with JavaLiteralArrayImpl {
  var typeContext: Option[Type] = None
}
final case class JavaInvocation(obj: Option[Expr], typeParams: Seq[Type], method: String, arguments: Seq[Expr], givenArgs: Seq[(String, Expr)], yields: Seq[(Expr, String)])(val blame: Blame[FrontendInvocationError])(implicit val o: Origin) extends JavaExpr with JavaInvocationImpl {
  var ref: Option[JavaInvocationTarget] = None
}
final case class JavaNewClass(args: Seq[Expr], typeArgs: Seq[Type], name: Type)(val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends JavaExpr with JavaNewClassImpl
final case class JavaNewLiteralArray(baseType: Type, dims: Int, initializer: Expr)(implicit val o: Origin) extends JavaExpr with JavaNewLiteralArrayImpl
final case class JavaNewDefaultArray(baseType: Type, specifiedDims: Seq[Expr], moreDims: Int)(implicit val o: Origin) extends JavaExpr with JavaNewDefaultArrayImpl

sealed trait PVLType extends Type with PVLTypeImpl
final case class PVLNamedType(name: String, typeArgs: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends PVLType with PVLNamedTypeImpl {
  var ref: Option[PVLTypeNameTarget] = None
}

sealed trait PVLExpr extends Expr with PVLExprImpl
final case class PVLLocal(name: String)(val blame: Blame[DerefInsufficientPermission])(implicit val o: Origin) extends PVLExpr with PVLLocalImpl {
  var ref: Option[PVLNameTarget] = None
}
final case class PVLDeref(obj: Expr, field: String)(val blame: Blame[FrontendDerefError])(implicit val o: Origin) extends PVLExpr with PVLDerefImpl {
  var ref: Option[PVLDerefTarget] = None
}
final case class PVLInvocation(obj: Option[Expr], method: String, args: Seq[Expr], typeArgs: Seq[Type], givenArgs: Seq[(String, Expr)], yields: Seq[(Expr, String)])(val blame: Blame[FrontendInvocationError])(implicit val o: Origin) extends PVLExpr with PVLInvocationImpl {
  var ref: Option[PVLInvocationTarget] = None
}

final case class PVLNew(t: Type, args: Seq[Expr])(val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends PVLExpr with PVLNewImpl

sealed trait PVLClassDeclaration extends ClassDeclaration with PVLClassDeclarationImpl
final class PVLConstructor(val contract: ApplicableContract, val args: Seq[Variable], val body: Option[Statement])(implicit val o: Origin) extends PVLClassDeclaration with PVLConstructorImpl

final case class SilverPredicateAccess(ref: Ref[Predicate], args: Seq[Expr], perm: Expr)(implicit val o: Origin) extends NodeFamily with SilverPredicateAccessImpl

sealed trait SilverExpr extends Expr with SilverExprImpl
final case class SilverDeref(obj: Expr, field: Ref[SilverField])(val blame: Blame[InsufficientPermission])(implicit val o: Origin) extends SilverExpr with HeapDeref with SilverDerefImpl

sealed trait SilverResource extends SilverExpr with SilverResourceImpl
final case class SilverPerm(obj: Expr, field: Ref[SilverField], perm: Expr)(implicit val o: Origin) extends SilverResource with SilverPermImpl
final case class SilverPredPerm(access: SilverPredicateAccess)(implicit val o: Origin) extends SilverResource with SilverPredPermImpl

final case class SilverUnfolding(access: SilverPredicateAccess, body: Expr)(implicit val o: Origin) extends SilverExpr with SilverUnfoldingImpl
final case class SilverCurFieldPerm(obj: Expr, field: Ref[SilverField])(implicit val o: Origin) extends SilverExpr with SilverCurFieldPermImpl
final case class SilverCurPredPerm(ref: Ref[Predicate], args: Seq[Expr])(implicit val o: Origin) extends SilverExpr with SilverCurPredPermImpl

sealed trait SilverStatement extends Statement with SilverStatementImpl
final case class SilverUnfold(access: SilverPredicateAccess)(val blame: Blame[SilverUnfoldFailed])(implicit val o: Origin) extends SilverStatement with SilverUnfoldImpl
final case class SilverFold(access: SilverPredicateAccess)(val blame: Blame[SilverFoldFailed])(implicit val o: Origin) extends SilverStatement with SilverFoldImpl
final case class SilverWhile(cond: Expr, invariant: Expr, body: Statement)(val blame: Blame[SilverWhileInvariantFailure])(implicit val o: Origin) extends SilverStatement with SilverWhileImpl
final case class SilverIf(cond: Expr, whenTrue: Statement, whenFalse: Statement)(implicit val o: Origin) extends SilverStatement with SilverIfImpl
final case class SilverNewRef(v: Ref[Variable], fields: Seq[Ref[SilverField]])(implicit val o: Origin) extends SilverStatement with SilverNewRefImpl

sealed trait SilverAssign extends SilverStatement with SilverAssignImpl
final case class SilverFieldAssign(obj: Expr, field: Ref[SilverField], value: Expr)(val blame: Blame[SilverAssignFailed])(implicit val o: Origin) extends SilverAssign with SilverFieldAssignImpl
final case class SilverLocalAssign(v: Ref[Variable], value: Expr)(implicit val o: Origin) extends SilverAssign with SilverLocalAssignImpl

sealed abstract class SilverDeclaration extends GlobalDeclaration with SilverDeclarationImpl
final class SilverField(val t: Type)(implicit val o: Origin) extends SilverDeclaration with SilverFieldImpl

