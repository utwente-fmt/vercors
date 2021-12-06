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

case class Program(declarations: Seq[GlobalDeclaration])(val blame: Blame[UnsafeCoercion])(implicit val o: Origin) extends NodeFamily with ProgramImpl

sealed trait Type extends NodeFamily with TypeImpl

final class TNotAValue(var decl: Option[Referrable])(implicit val o: Origin = DiagnosticOrigin) extends Type with TNotAValueImpl
case class TUnion(types: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends Type with TUnionImpl
case class TArray(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends Type with TArrayImpl
case class TPointer(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends Type with TPointerImpl
case class TType(t: Type)(implicit val o: Origin = DiagnosticOrigin) extends Type with TTypeImpl
case class TVar(ref: Ref[Variable])(implicit val o: Origin = DiagnosticOrigin) extends Type with TVarImpl

sealed trait CompositeType extends Type with CompositeTypeImpl
sealed trait SizedType extends CompositeType with SizedTypeImpl
case class TSeq(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends SizedType with TSeqImpl
case class TSet(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends SizedType with TSetImpl
case class TBag(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends SizedType with TBagImpl

case class TOption(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends CompositeType with TOptionImpl
case class TTuple(elements: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends CompositeType with TTupleImpl
case class TEither(left: Type, right: Type)(implicit val o: Origin = DiagnosticOrigin) extends CompositeType with TEitherImpl
case class TMatrix(element: Type)(implicit val o: Origin = DiagnosticOrigin) extends CompositeType with TMatrixImpl
case class TMap(key: Type, value: Type)(implicit val o: Origin = DiagnosticOrigin) extends CompositeType with TMapImpl

sealed trait PrimitiveType extends Type with PrimitiveTypeImpl
case class TAny()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TAnyImpl
case class TNothing()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TNothingImpl
case class TVoid()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TVoidImpl
case class TNull()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TNullImpl
case class TBool()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TBoolImpl
case class TResource()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TResourceImpl
case class TChar()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TCharImpl
case class TString()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TStringImpl
case class TRef()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TRefImpl
case class TProcess()(implicit val o: Origin = DiagnosticOrigin) extends PrimitiveType with TProcessImpl

sealed trait NumericType extends PrimitiveType with NumericTypeImpl
case class TInt()(implicit val o: Origin = DiagnosticOrigin) extends NumericType with TIntImpl
case class TBoundedInt(gte: BigInt, lt: BigInt)(implicit val o: Origin = DiagnosticOrigin) extends NumericType with TBoundedIntImpl
case class TFloat()(implicit val o: Origin = DiagnosticOrigin) extends NumericType with TFloatImpl
case class TRational()(implicit val o: Origin = DiagnosticOrigin) extends NumericType with TRationalImpl
case class TFraction()(implicit val o: Origin = DiagnosticOrigin) extends NumericType with TFractionImpl
case class TZFraction()(implicit val o: Origin = DiagnosticOrigin) extends NumericType with TZFractionImpl

sealed trait DeclaredType extends Type with DeclaredTypeImpl
case class TModel(model: Ref[Model])(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType with TModelImpl
case class TClass(cls: Ref[Class])(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType with TClassImpl
case class TAxiomatic(adt: Ref[AxiomaticDataType], args: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends DeclaredType with TAxiomaticImpl

sealed trait ParRegion extends NodeFamily with ParRegionImpl
case class ParParallel(regions: Seq[ParRegion])(val blame: Blame[ParPreconditionFailed])(implicit val o: Origin) extends ParRegion with ParParallelImpl
case class ParSequential(regions: Seq[ParRegion])(val blame: Blame[ParPreconditionFailed])(implicit val o: Origin) extends ParRegion with ParSequentialImpl
case class ParBlock(decl: ParBlockDecl, iters: Seq[IterVariable], requires: Expr, ensures: Expr, content: Statement)(val blame: Blame[ParBlockFailure])(implicit val o: Origin) extends ParRegion with ParBlockImpl

sealed trait LoopContract extends NodeFamily with LoopContractImpl
case class LoopInvariant(invariant: Expr)(implicit val o: Origin) extends LoopContract with LoopInvariantImpl
case class IterationContract(requires: Expr, ensures: Expr)(implicit val o: Origin) extends LoopContract with IterationContractImpl

case class CatchClause(decl: Variable, body: Statement)(implicit val o: Origin) extends NodeFamily with CatchClauseImpl

case class IterVariable(variable: Variable, from: Expr, to: Expr)(implicit val o: Origin) extends NodeFamily with IterVariableImpl

sealed trait Statement extends NodeFamily with StatementImpl

sealed trait NonExecutableStatement extends Statement with NonExecutableStatementImpl
case class LocalDecl(local: Variable)(implicit val o: Origin) extends NonExecutableStatement with LocalDeclImpl
case class SpecIgnoreStart()(implicit val o: Origin) extends NonExecutableStatement with SpecIgnoreStartImpl
case class SpecIgnoreEnd()(implicit val o: Origin) extends NonExecutableStatement with SpecIgnoreEndImpl

sealed trait NormallyCompletingStatement extends Statement with NormallyCompletingStatementImpl
case class Assign(target: Expr, value: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with AssignImpl
case class Send(resource: Expr, label: Ref[LabelDecl], offset: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with SendImpl
case class Recv(resource: Expr, label: Ref[LabelDecl], offset: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with RecvImpl
sealed trait SwitchCase extends NormallyCompletingStatement with SwitchCaseImpl
case class DefaultCase()(implicit val o: Origin) extends SwitchCase with DefaultCaseImpl
case class Case(pattern: Expr)(implicit val o: Origin) extends SwitchCase with CaseImpl
case class Label(decl: LabelDecl, stat: Statement)(implicit val o: Origin) extends NormallyCompletingStatement with LabelImpl
case class Goto(lbl: Ref[LabelDecl])(implicit val o: Origin) extends NormallyCompletingStatement with GotoImpl
case class Exhale(res: Expr)(val blame: Blame[ExhaleFailed])(implicit val o: Origin) extends NormallyCompletingStatement with ExhaleImpl
case class Assert(res: Expr)(val blame: Blame[AssertFailed])(implicit val o: Origin) extends NormallyCompletingStatement with AssertImpl
case class Refute(assn: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with RefuteImpl
case class Inhale(res: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with InhaleImpl
case class Assume(assn: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with AssumeImpl
case class Wait(obj: Expr)(val blame: Blame[UnlockFailure])(implicit val o: Origin) extends NormallyCompletingStatement with WaitImpl
case class Notify(obj: Expr)(val blame: Blame[NotifyFailed])(implicit val o: Origin) extends NormallyCompletingStatement with NotifyImpl
case class Fork(obj: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with ForkImpl
case class Join(obj: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with JoinImpl
case class Lock(obj: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with LockImpl
case class Unlock(obj: Expr)(val blame: Blame[UnlockFailure])(implicit val o: Origin) extends NormallyCompletingStatement with UnlockImpl
case class Commit(obj: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with CommitImpl
case class Fold(res: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with FoldImpl
case class Unfold(res: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with UnfoldImpl
case class WandQed(res: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with WandQedImpl
case class WandApply(res: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with WandApplyImpl
case class WandUse(res: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with WandUseImpl
case class Havoc(loc: Expr)(implicit val o: Origin) extends NormallyCompletingStatement with HavocImpl

sealed trait ExceptionalStatement extends Statement with ExceptionalStatementImpl
case class Eval(expr: Expr)(implicit val o: Origin) extends ExceptionalStatement with EvalImpl
case class Return(result: Expr)(implicit val o: Origin) extends ExceptionalStatement with ReturnImpl
case class Throw(obj: Expr)(val blame: Blame[ThrowNull])(implicit val o: Origin) extends ExceptionalStatement with ThrowImpl
case class Break(label: Option[Ref[LabelDecl]])(implicit val o: Origin) extends ExceptionalStatement with BreakImpl
case class Continue(label: Option[Ref[LabelDecl]])(implicit val o: Origin) extends ExceptionalStatement with ContinueImpl

sealed trait CompositeStatement extends Statement with CompositeStatementImpl
case class Block(statements: Seq[Statement])(implicit val o: Origin) extends Statement with BlockImpl
case class Scope(locals: Seq[Variable], body: Statement)(implicit val o: Origin) extends Statement with ScopeImpl
case class Branch(branches: Seq[(Expr, Statement)])(implicit val o: Origin) extends Statement with BranchImpl
case class Switch(expr: Expr, body: Statement)(implicit val o: Origin) extends Statement with SwitchImpl
case class Loop(init: Statement, cond: Expr, update: Statement, contract: LoopContract, body: Statement)(implicit val o: Origin) extends Statement with LoopImpl
case class TryCatchFinally(body: Statement, after: Statement, catches: Seq[CatchClause])(implicit val o: Origin) extends Statement with TryCatchFinallyImpl
case class Synchronized(obj: Expr, body: Statement)(val blame: Blame[UnlockFailure])(implicit val o: Origin) extends Statement with SynchronizedImpl
case class ParInvariant(decl: ParInvariantDecl, inv: Expr, content: Statement)(val blame: Blame[ParInvariantNotEstablished])(implicit val o: Origin) extends Statement with ParInvariantImpl
case class ParAtomic(inv: Seq[Ref[ParInvariantDecl]], content: Statement)(implicit val o: Origin) extends Statement with ParAtomicImpl
case class ParBarrier(block: Ref[ParBlockDecl], invs: Seq[Ref[ParInvariantDecl]], requires: Expr, ensures: Expr, content: Statement)(val blame: Blame[ParBarrierFailed])(implicit val o: Origin) extends Statement with ParBarrierImpl
case class ParStatement(impl: ParRegion)(val blame: Blame[Nothing])(implicit val o: Origin) extends Statement with ParStatementImpl
case class VecBlock(iters: Seq[IterVariable], requires: Expr, ensures: Expr, content: Statement)(implicit val o: Origin) extends Statement with VecBlockImpl
case class WandCreate(statements: Seq[Statement])(implicit val o: Origin) extends Statement with WandCreateImpl
case class ModelDo(model: Expr, perm: Expr, after: Expr, action: Expr, impl: Statement)(implicit val o: Origin) extends Statement with ModelDoImpl

sealed abstract class Declaration extends Node with DeclarationImpl {
  var debugRewriteState: DebugRewriteState = NotProcessed
}

sealed abstract class GlobalDeclaration extends Declaration with GlobalDeclarationImpl
class SimplificationRule(val axiom: Expr)(implicit val o: Origin) extends GlobalDeclaration with SimplificationRuleImpl
class AxiomaticDataType(val decls: Seq[ADTDeclaration], val typeArgs: Seq[Variable])(implicit val o: Origin) extends GlobalDeclaration with AxiomaticDataTypeImpl
class Class(val declarations: Seq[ClassDeclaration], val supports: Seq[Ref[Class]], val intrinsicLockInvariant: Expr)(implicit val o: Origin) extends GlobalDeclaration with ClassImpl
class Model(val declarations: Seq[ModelDeclaration])(implicit val o: Origin) extends GlobalDeclaration with Declarator with ModelImpl
class Function(val returnType: Type, val args: Seq[Variable], val typeArgs: Seq[Variable],
               val body: Option[Expr], val contract: ApplicableContract, val inline: Boolean = false)
              (val blame: Blame[PostconditionFailed])(implicit val o: Origin)
  extends GlobalDeclaration with AbstractFunction with FunctionImpl
class Procedure(val returnType: Type,
                val args: Seq[Variable], val outArgs: Seq[Variable], val typeArgs: Seq[Variable],
                val body: Option[Statement],
                val contract: ApplicableContract,
                val inline: Boolean = false, val pure: Boolean = false)
               (val blame: Blame[PostconditionFailed])(implicit val o: Origin)
  extends GlobalDeclaration with AbstractMethod with ProcedureImpl
class Predicate(val args: Seq[Variable], val body: Option[Expr],
                val threadLocal: Boolean = false, val inline: Boolean = false)(implicit val o: Origin)
  extends GlobalDeclaration with AbstractPredicate with PredicateImpl

sealed abstract class ClassDeclaration extends Declaration with ClassDeclarationImpl
class InstanceFunction(val returnType: Type, val args: Seq[Variable], val typeArgs: Seq[Variable],
                       val body: Option[Expr], val contract: ApplicableContract, val inline: Boolean)
                      (val blame: Blame[PostconditionFailed])(implicit val o: Origin)
  extends ClassDeclaration with AbstractFunction with InstanceFunctionImpl
class InstanceMethod(val returnType: Type,
                     val args: Seq[Variable], val outArgs: Seq[Variable], val typeArgs: Seq[Variable],
                     val body: Option[Statement],
                     val contract: ApplicableContract,
                     val inline: Boolean = false, val pure: Boolean = false)
                    (val blame: Blame[PostconditionFailed])(implicit val o: Origin)
  extends ClassDeclaration with AbstractMethod with InstanceMethodImpl
class InstancePredicate(val args: Seq[Variable], val body: Option[Expr],
                        val threadLocal: Boolean = false, val inline: Boolean = false)(implicit val o: Origin)
  extends ClassDeclaration with AbstractPredicate with InstancePredicateImpl
class InstanceField(val t: Type, val flags: Set[FieldFlag])(implicit val o: Origin) extends ClassDeclaration with Field with InstanceFieldImpl

sealed trait ModelDeclaration extends Declaration with ModelDeclarationImpl
class ModelField(val t: Type)(implicit val o: Origin) extends ModelDeclaration with Field with ModelFieldImpl
class ModelProcess(val args: Seq[Variable], val impl: Expr,
                   val requires: Expr, val ensures: Expr,
                   val modifies: Seq[Ref[ModelField]], val accessible: Seq[Ref[ModelField]])
                  (val blame: Blame[PostconditionFailed])
                  (implicit val o: Origin) extends ModelDeclaration with Applicable with ModelProcessImpl
class ModelAction(val args: Seq[Variable],
                  val requires: Expr, val ensures: Expr,
                  val modifies: Seq[Ref[ModelField]], val accessible: Seq[Ref[ModelField]])
                 (implicit val o: Origin) extends ModelDeclaration with Applicable with ModelActionImpl

sealed trait ADTDeclaration extends Declaration with ADTDeclarationImpl
class ADTAxiom(val axiom: Expr)(implicit val o: Origin) extends ADTDeclaration with ADTAxiomImpl
class ADTFunction(val args: Seq[Variable], val returnType: Type)(implicit val o: Origin) extends Applicable with ADTDeclaration with ADTFunctionImpl

class Variable(val t: Type)(implicit val o: Origin) extends Declaration with VariableImpl
class LabelDecl()(implicit val o: Origin) extends Declaration with LabelDeclImpl
class ParBlockDecl()(implicit val o: Origin) extends Declaration with ParBlockDeclImpl
class ParInvariantDecl()(implicit val o: Origin) extends Declaration with ParInvariantDeclImpl

sealed trait Applicable extends Declaration with ApplicableImpl
sealed trait InlineableApplicable extends Applicable with InlineableApplicableImpl
sealed trait AbstractPredicate extends InlineableApplicable with AbstractPredicateImpl
sealed trait ContractApplicable extends InlineableApplicable with ContractApplicableImpl
sealed trait AbstractFunction extends ContractApplicable with AbstractFunctionImpl
sealed trait AbstractMethod extends ContractApplicable with AbstractMethodImpl
sealed trait Field extends FieldImpl

case class SignalsClause(binding: Variable, assn: Expr)(implicit val o: Origin) extends NodeFamily with SignalsClauseImpl

case class ApplicableContract(requires: Expr, ensures: Expr, contextEverywhere: Expr,
                              signals: Seq[SignalsClause], givenArgs: Seq[Variable], yieldsArgs: Seq[Variable])
                             (implicit val o: Origin) extends NodeFamily with ApplicableContractImpl

sealed trait FieldFlag extends NodeFamily with FieldFlagImpl
class Final()(implicit val o: Origin) extends FieldFlag with FinalImpl

sealed trait Expr extends NodeFamily with ExprImpl

sealed abstract class Constant[T] extends Expr with ConstantImpl[T]
case class IntegerValue(value: BigInt)(implicit val o: Origin) extends Constant[BigInt] with Expr with IntegerValueImpl
case class BooleanValue(value: Boolean)(implicit val o: Origin) extends Constant[Boolean] with BooleanValueImpl
case class LiteralSeq(element: Type, values: Seq[Expr])(implicit val o: Origin) extends Expr with LiteralSeqImpl
case class LiteralSet(element: Type, values: Seq[Expr])(implicit val o: Origin) extends Expr with LiteralSetImpl
case class LiteralBag(element: Type, values: Seq[Expr])(implicit val o: Origin) extends Expr with LiteralBagImpl
case class LiteralTuple(ts: Seq[Type], values: Seq[Expr])(implicit val o: Origin) extends Expr with LiteralTupleImpl
case class LiteralMap(k: Type, v: Type, values: Seq[(Expr, Expr)])(implicit val o: Origin) extends Expr with LiteralMapImpl
case class UntypedLiteralSeq(values: Seq[Expr])(implicit val o: Origin) extends Expr with UntypedLiteralSeqImpl
case class UntypedLiteralSet(values: Seq[Expr])(implicit val o: Origin) extends Expr with UntypedLiteralSetImpl
case class UntypedLiteralBag(values: Seq[Expr])(implicit val o: Origin) extends Expr with UntypedLiteralBagImpl
case class Void()(implicit val o: Origin) extends Expr with VoidImpl
case class Null()(implicit val o: Origin) extends Expr with NullImpl
case class NoPerm()(implicit val o: Origin) extends Expr with NoPermImpl
case class WritePerm()(implicit val o: Origin) extends Expr with WritePermImpl
case class OptSome(e: Expr)(implicit val o: Origin) extends Expr with OptSomeImpl
case class OptNone()(implicit val o: Origin) extends Expr with OptNoneImpl
case class Range(from: Expr, to: Expr)(implicit val o: Origin) extends Expr with RangeImpl
case class EitherLeft(e: Expr)(implicit val o: Origin) extends Expr with EitherLeftImpl
case class EitherRight(e: Expr)(implicit val o: Origin) extends Expr with EitherRightImpl
case class MapCons(map: Expr, k: Expr, v: Expr)(implicit val o: Origin) extends Expr with MapConsImpl

case class AmbiguousThis()(implicit val o: Origin) extends Expr with AmbiguousThisImpl {
  var ref: Option[ThisTarget] = None
}

case class ThisObject(cls: Ref[Class])(implicit val o: Origin) extends Expr with ThisObjectImpl
case class ThisModel(cls: Ref[Model])(implicit val o: Origin) extends Expr with ThisModelImpl

case class AmbiguousResult()(implicit val o: Origin) extends Expr with AmbiguousResultImpl {
  var ref: Option[ResultTarget] = None
}

case class Result(applicable: Ref[ContractApplicable])(implicit val o: Origin) extends Expr with ResultImpl
case class CurrentThreadId()(implicit val o: Origin) extends Expr with CurrentThreadIdImpl
case class Any()(implicit val o: Origin) extends Expr with AnyImpl
case class ReadPerm()(implicit val o: Origin) extends Expr with ReadPermImpl
case class Values(arr: Expr, from: Expr, to: Expr)(val blame: Blame[ArrayValuesError])(implicit val o: Origin) extends Expr with ValuesImpl
case class MapEq(left: Expr, right: Expr)(implicit val o: Origin) extends Expr with MapEqImpl
case class MapDisjoint(left: Expr, right: Expr)(implicit val o: Origin) extends Expr with MapDisjointImpl
sealed trait MapOp extends Expr with MapOpImpl
case class MapKeySet(map: Expr)(implicit val o: Origin) extends MapOp with MapKeySetImpl
case class MapValueSet(map: Expr)(implicit val o: Origin) extends MapOp with MapValueSetImpl
case class MapItemSet(map: Expr)(implicit val o: Origin) extends MapOp with MapItemSetImpl
case class MapSize(map: Expr)(implicit val o: Origin) extends MapOp with MapSizeImpl
case class MapRemove(map: Expr, k: Expr)(implicit val o: Origin) extends Expr with MapRemoveImpl

sealed trait Binder extends Expr with Declarator with BinderImpl
case class Forall(bindings: Seq[Variable], triggers: Seq[Seq[Expr]], body: Expr)(implicit val o: Origin) extends Binder with ForallImpl
case class Starall(bindings: Seq[Variable], triggers: Seq[Seq[Expr]], body: Expr)(implicit val o: Origin) extends Binder with StarallImpl
case class Exists(bindings: Seq[Variable], triggers: Seq[Seq[Expr]], body: Expr)(implicit val o: Origin) extends Binder with ExistsImpl
case class Sum(bindings: Seq[Variable], condition: Expr, main: Expr)(implicit val o: Origin) extends Binder with SumImpl
case class Product(bindings: Seq[Variable], condition: Expr, main: Expr)(implicit val o: Origin) extends Binder with ProductImpl
case class Let(binding: Variable, value: Expr, main: Expr)(implicit val o: Origin) extends Binder with LetImpl
case class InlinePattern(inner: Expr)(implicit val o: Origin) extends Expr with InlinePatternImpl

case class Local(ref: Ref[Variable])(implicit val o: Origin) extends Expr with LocalImpl
sealed trait HeapDeref extends HeapDerefImpl
case class Deref(obj: Expr, ref: Ref[InstanceField])(val blame: Blame[InsufficientPermission])(implicit val o: Origin) extends Expr with HeapDeref with DerefImpl
case class ModelDeref(obj: Expr, ref: Ref[ModelField])(val blame: Blame[ModelInsufficientPermission])(implicit val o: Origin) extends Expr with ModelDerefImpl
case class DerefPointer(pointer: Expr)(val blame: Blame[PointerDerefError])(implicit val o: Origin) extends Expr with DerefPointerImpl
case class PointerAdd(pointer: Expr, offset: Expr)(val blame: Blame[PointerAddError])(implicit val o: Origin) extends Expr with PointerAddImpl
case class AddrOf(e: Expr)(implicit val o: Origin) extends Expr with AddrOfImpl

sealed trait Apply extends Expr with ApplyImpl
case class ADTFunctionInvocation(typeArgs: Option[(Ref[AxiomaticDataType], Seq[Type])], ref: Ref[ADTFunction], args: Seq[Expr])(implicit val o: Origin) extends Apply with ADTFunctionInvocationImpl

sealed trait ApplyInlineable extends Apply with ApplyInlineableImpl

sealed trait ApplyAnyPredicate extends ApplyInlineable with ApplyAnyPredicateImpl
case class PredicateApply(ref: Ref[Predicate], args: Seq[Expr])(implicit val o: Origin) extends ApplyAnyPredicate with PredicateApplyImpl
case class InstancePredicateApply(obj: Expr, ref: Ref[InstancePredicate], args: Seq[Expr])(implicit val o: Origin) extends ApplyAnyPredicate with InstancePredicateApplyImpl

sealed trait Invocation extends ApplyInlineable with InvocationImpl

sealed trait AnyMethodInvocation extends Invocation with AnyMethodInvocationImpl
case class ProcedureInvocation(ref: Ref[Procedure], args: Seq[Expr], outArgs: Seq[Ref[Variable]], typeArgs: Seq[Type])(val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends AnyMethodInvocation with ProcedureInvocationImpl
case class MethodInvocation(obj: Expr, ref: Ref[InstanceMethod], args: Seq[Expr], outArgs: Seq[Ref[Variable]], typeArgs: Seq[Type])(val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends AnyMethodInvocation with MethodInvocationImpl

sealed trait AnyFunctionInvocation extends Invocation with AnyFunctionInvocationImpl
case class FunctionInvocation(ref: Ref[Function], args: Seq[Expr], typeArgs: Seq[Type])(val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends AnyFunctionInvocation with FunctionInvocationImpl
case class InstanceFunctionInvocation(obj: Expr, ref: Ref[InstanceFunction], args: Seq[Expr], typeArgs: Seq[Type])(val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends AnyFunctionInvocation with InstanceFunctionInvocationImpl

sealed trait UnExpr extends Expr with UnExprImpl

case class UMinus(arg: Expr)(implicit val o: Origin) extends UnExpr with UMinusImpl
case class BitNot(arg: Expr)(implicit val o: Origin) extends UnExpr with BitNotImpl
case class Not(arg: Expr)(implicit val o: Origin) extends UnExpr with NotImpl

sealed trait BinExpr extends Expr with BinExprImpl
sealed trait NumericBinExpr extends BinExpr with NumericBinExprImpl

sealed trait DividingExpr extends Expr with DividingExprImpl

case class AmbiguousMult(left: Expr, right: Expr)(implicit val o: Origin) extends Expr with AmbiguousMultImpl
case class AmbiguousPlus(left: Expr, right: Expr)(val blame: Blame[FrontendPlusError])(implicit val o: Origin) extends Expr with AmbiguousPlusImpl
case class AmbiguousOr(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with AmbiguousOrImpl

sealed trait BitOp extends BinExpr with BitOpImpl
case class AmbiguousComputationalOr(left: Expr, right: Expr)(implicit val o: Origin) extends BitOp with AmbiguousComputationalOrImpl
case class AmbiguousComputationalXor(left: Expr, right: Expr)(implicit val o: Origin) extends BitOp with AmbiguousComputationalXorImpl
case class AmbiguousComputationalAnd(left: Expr, right: Expr)(implicit val o: Origin) extends BitOp with AmbiguousComputationalAndImpl

case class ComputationalOr(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with ComputationalOrImpl
case class ComputationalXor(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with ComputationalXorImpl
case class ComputationalAnd(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with ComputationalAndImpl

case class Exp(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr with ExpImpl
case class Plus(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr with PlusImpl
case class Minus(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr with MinusImpl
case class Mult(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr with MultImpl
case class Div(left: Expr, right: Expr)(val blame: Blame[DivByZero])(implicit val o: Origin) extends BinExpr with DividingExpr with DivImpl
case class FloorDiv(left: Expr, right: Expr)(val blame: Blame[DivByZero])(implicit val o: Origin) extends BinExpr with DividingExpr with FloorDivImpl
case class Mod(left: Expr, right: Expr)(val blame: Blame[DivByZero])(implicit val o: Origin) extends NumericBinExpr with DividingExpr with ModImpl

case class BitAnd(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with BitAndImpl
case class BitOr(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with BitOrImpl
case class BitXor(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with BitXorImpl
case class BitShl(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with BitShlImpl
case class BitShr(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with BitShrImpl
case class BitUShr(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with BitUShrImpl

case class And(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with AndImpl
case class Or(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with OrImpl
case class Implies(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with ImpliesImpl
case class Star(left: Expr, right: Expr)(implicit val o: Origin) extends Expr with StarImpl
case class Wand(left: Expr, right: Expr)(implicit val o: Origin) extends Expr with WandImpl
case class Scale(scale: Expr, res: Expr)(implicit val o: Origin) extends Expr with ScaleImpl

case class Unfolding(pred: Expr, body: Expr)(implicit val o: Origin) extends Expr with UnfoldingImpl

case class Perm(loc: Expr, perm: Expr)(implicit val o: Origin) extends Expr with PermImpl
case class HPerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends Expr with HPermImpl
case class APerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends Expr with APermImpl
case class PointsTo(loc: Expr, perm: Expr, value: Expr)(implicit val o: Origin) extends Expr with PointsToImpl

case class CurPerm(loc: Expr)(implicit val o: Origin) extends Expr with CurPermImpl

case class ValidArray(arr: Expr, len: Expr)(implicit val o: Origin) extends Expr with ValidArrayImpl
case class ValidMatrix(mat: Expr, w: Expr, h: Expr)(implicit val o: Origin) extends Expr with ValidMatrixImpl

case class PermPointer(p: Expr, len: Expr, perm: Expr)(implicit val o: Origin) extends Expr with PermPointerImpl
case class PermPointerIndex(p: Expr, idx: Expr, perm: Expr)(implicit val o: Origin) extends Expr with PermPointerIndexImpl

sealed trait Comparison extends BinExpr with ComparisonImpl
case class Eq(left: Expr, right: Expr)(implicit val o: Origin) extends Comparison with EqImpl
case class Neq(left: Expr, right: Expr)(implicit val o: Origin) extends Comparison with NeqImpl

sealed trait OrderOp extends Comparison with OrderOpImpl
case class Greater(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp with GreaterImpl
case class Less(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp with LessImpl
case class GreaterEq(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp with GreaterEqImpl
case class LessEq(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp with LessEqImpl

case class Select(condition: Expr, whenTrue: Expr, whenFalse: Expr)(implicit val o: Origin) extends Expr with SelectImpl
case class NewObject(cls: Ref[Class])(implicit val o: Origin) extends Expr with NewObjectImpl
case class NewArray(element: Type, dims: Seq[Expr], moreDims: Int)(implicit val o: Origin) extends Expr with NewArrayImpl
case class Old(expr: Expr, at: Option[Ref[LabelDecl]])(val blame: Blame[LabelNotReached])(implicit val o: Origin) extends Expr with OldImpl
case class AmbiguousSubscript(collection: Expr, index: Expr)(val blame: Blame[FrontendSubscriptError])(implicit val o: Origin) extends Expr with AmbiguousSubscriptImpl
case class SeqSubscript(seq: Expr, index: Expr)(val blame: Blame[SeqBoundFailure])(implicit val o: Origin) extends Expr with SeqSubscriptImpl
case class ArraySubscript(arr: Expr, index: Expr)(val blame: Blame[ArraySubscriptError])(implicit val o: Origin) extends Expr with ArraySubscriptImpl
case class PointerSubscript(pointer: Expr, index: Expr)(val blame: Blame[PointerSubscriptError])(implicit val o: Origin) extends Expr with PointerSubscriptImpl
case class Length(arr: Expr)(val blame: Blame[ArrayNull])(implicit val o: Origin) extends Expr with LengthImpl
case class Size(obj: Expr)(implicit val o: Origin) extends Expr with SizeImpl
case class Cons(x: Expr, xs: Expr)(implicit val o: Origin) extends Expr with ConsImpl

case class Head(xs: Expr)(val blame: Blame[SeqBoundFailure])(implicit val o: Origin) extends Expr with HeadImpl
case class Tail(xs: Expr)(implicit val o: Origin) extends Expr with TailImpl
case class Drop(xs: Expr, count: Expr)(implicit val o: Origin) extends Expr with DropImpl
case class Take(xs: Expr, count: Expr)(implicit val o: Origin) extends Expr with TakeImpl
case class Slice(xs: Expr, from: Expr, to: Expr)(implicit val o: Origin) extends Expr with SliceImpl
case class SeqUpdate(xs: Expr, i: Expr, x: Expr)(implicit val o: Origin) extends Expr with SeqUpdateImpl
case class Concat(xs: Expr, ys: Expr)(implicit val o: Origin) extends Expr with ConcatImpl
case class RemoveAt(xs: Expr, i: Expr)(implicit val o: Origin) extends Expr with RemoveAtImpl
case class Empty(obj: Expr)(implicit val o: Origin) extends Expr with EmptyImpl

case class AmbiguousMember(x: Expr, xs: Expr)(implicit val o: Origin) extends Expr with AmbiguousMemberImpl
case class SetMember(x: Expr, xs: Expr)(implicit val o: Origin) extends Expr with SetMemberImpl
case class SeqMember(x: Expr, xs: Expr)(implicit val o: Origin) extends Expr with SeqMemberImpl
case class MapMember(x: Expr, xs: Expr)(implicit val o: Origin) extends Expr with MapMemberImpl
case class BagMemberCount(x: Expr, xs: Expr)(implicit val o: Origin) extends Expr with BagMemberCountImpl

sealed trait SetComparison extends Comparison with SetComparisonImpl
case class SubSet(left: Expr, right: Expr)(implicit val o: Origin) extends SetComparison with SubSetImpl
case class SubSetEq(left: Expr, right: Expr)(implicit val o: Origin) extends SetComparison with SubSetEqImpl
case class Permutation(xs: Expr, ys: Expr)(implicit val o: Origin) extends Expr with PermutationImpl
case class OptGet(opt: Expr)(val blame: Blame[OptionNone])(implicit val o: Origin) extends Expr with OptGetImpl
case class OptGetOrElse(opt: Expr, alt: Expr)(implicit val o: Origin) extends Expr with OptGetOrElseImpl
case class MapGet(map: Expr, k: Expr)(val blame: Blame[MapKeyError])(implicit val o: Origin) extends Expr with MapGetImpl
case class TupGet(tup: Expr, index: Int)(implicit val o: Origin) extends Expr with TupGetImpl

sealed trait EitherOp extends Expr with EitherOpImpl
case class GetLeft(either: Expr)(val blame: Blame[NotLeft])(implicit val o: Origin) extends EitherOp with GetLeftImpl
case class GetRight(either: Expr)(val blame: Blame[NotRight])(implicit val o: Origin) extends EitherOp with GetRightImpl
case class IsLeft(either: Expr)(implicit val o: Origin) extends EitherOp with Expr with IsLeftImpl
case class IsRight(either: Expr)(implicit val o: Origin) extends EitherOp with Expr with IsRightImpl

case class VectorSum(indices: Expr, vec: Expr)(implicit val o: Origin) extends Expr with VectorSumImpl
case class VectorCompare(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with VectorCompareImpl
case class VectorRepeat(e: Expr)(implicit val o: Origin) extends Expr with VectorRepeatImpl
case class MatrixSum(indices: Expr, mat: Expr)(implicit val o: Origin) extends Expr with MatrixSumImpl
case class MatrixCompare(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with MatrixCompareImpl
case class MatrixRepeat(e: Expr)(implicit val o: Origin) extends Expr with MatrixRepeatImpl

case class TypeValue(value: Type)(implicit val o: Origin) extends Expr with TypeValueImpl
case class TypeOf(expr: Expr)(implicit val o: Origin) extends Expr with TypeOfImpl
case class InstanceOf(value: Expr, typeValue: Expr)(implicit val o: Origin) extends Expr with InstanceOfImpl
case class Cast(value: Expr, typeValue: Expr)(implicit val o: Origin) extends Expr with CastImpl

sealed trait TypeComparison extends Comparison with TypeComparisonImpl
case class SubType(left: Expr, right: Expr)(implicit val o: Origin) extends TypeComparison with SubTypeImpl
case class SuperType(left: Expr, right: Expr)(implicit val o: Origin) extends TypeComparison with SuperTypeImpl

sealed trait AssignExpression extends Expr with AssignExpressionImpl
case class PreAssignExpression(target: Expr, value: Expr)(implicit val o: Origin) extends AssignExpression with PreAssignExpressionImpl
case class PostAssignExpression(target: Expr, value: Expr)(implicit val o: Origin) extends AssignExpression with PostAssignExpressionImpl

case class With(pre: Statement, value: Expr)(implicit val o: Origin) extends Expr with WithImpl
case class Then(value: Expr, post: Statement)(implicit val o: Origin) extends Expr with ThenImpl

case class Held(obj: Expr)(implicit val o: Origin) extends Expr with HeldImpl
case class IdleToken(thread: Expr)(implicit val o: Origin) extends Expr with IdleTokenImpl
case class JoinToken(thread: Expr)(implicit val o: Origin) extends Expr with JoinTokenImpl

case class EmptyProcess()(implicit val o: Origin) extends Expr with EmptyProcessImpl
case class ActionApply(action: Ref[ModelAction], args: Seq[Expr])(implicit val o: Origin) extends Expr with ActionApplyImpl
case class ProcessApply(process: Ref[ModelProcess], args: Seq[Expr])(implicit val o: Origin) extends Expr with ProcessApplyImpl
case class ProcessSeq(left: Expr, right: Expr)(implicit val o: Origin) extends Expr with ProcessSeqImpl
case class ProcessChoice(left: Expr, right: Expr)(implicit val o: Origin) extends Expr with ProcessChoiceImpl
case class ProcessPar(left: Expr, right: Expr)(implicit val o: Origin) extends Expr with ProcessParImpl
case class ProcessSelect(cond: Expr, whenTrue: Expr, whenFalse: Expr)(implicit val o: Origin) extends Expr with ProcessSelectImpl

case class ModelNew(ref: Ref[Model])(implicit val o: Origin) extends Expr with ModelNewImpl

case class ModelState(model: Expr, perm: Expr, state: Expr)(implicit val o: Origin) extends Expr with ModelStateImpl
case class ModelAbstractState(model: Expr, state: Expr)(implicit val o: Origin) extends Expr with ModelAbstractStateImpl
case class ModelCreate(model: Expr, init: Expr)(implicit val o: Origin) extends Expr with ModelCreateImpl
case class ModelDestroy(model: Expr)(implicit val o: Origin) extends Expr with ModelDestroyImpl
case class ModelSplit(model: Expr, leftPerm: Expr, leftProcess: Expr, rightPerm: Expr, rightProcess: Expr)(implicit val o: Origin) extends Expr with ModelSplitImpl
case class ModelMerge(model: Expr, leftPerm: Expr, leftProcess: Expr, rightPerm: Expr, rightProcess: Expr)(implicit val o: Origin) extends Expr with ModelMergeImpl
case class ModelChoose(model: Expr, perm: Expr, totalProcess: Expr, choice: Expr)(implicit val o: Origin) extends Expr with ModelChooseImpl

case class ModelPerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends Expr with ModelPermImpl
case class ActionPerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends Expr with ActionPermImpl

sealed trait CDeclarationSpecifier extends NodeFamily with CDeclarationSpecifierImpl

sealed trait CSpecificationModifier extends CDeclarationSpecifier with CSpecificationModifierImpl
case class CPure()(implicit val o: Origin) extends CSpecificationModifier with CPureImpl
case class CInline()(implicit val o: Origin) extends CSpecificationModifier with CInlineImpl

sealed trait CStorageClassSpecifier extends CDeclarationSpecifier with CStorageClassSpecifierImpl
case class CTypedef()(implicit val o: Origin) extends CStorageClassSpecifier with CTypedefImpl
case class CExtern()(implicit val o: Origin) extends CStorageClassSpecifier with CExternImpl
case class CStatic()(implicit val o: Origin) extends CStorageClassSpecifier with CStaticImpl

sealed trait CTypeSpecifier extends CDeclarationSpecifier with CTypeSpecifierImpl
case class CVoid()(implicit val o: Origin) extends CTypeSpecifier with CVoidImpl
case class CChar()(implicit val o: Origin) extends CTypeSpecifier with CCharImpl
case class CShort()(implicit val o: Origin) extends CTypeSpecifier with CShortImpl
case class CInt()(implicit val o: Origin) extends CTypeSpecifier with CIntImpl
case class CLong()(implicit val o: Origin) extends CTypeSpecifier with CLongImpl
case class CFloat()(implicit val o: Origin) extends CTypeSpecifier with CFloatImpl
case class CDouble()(implicit val o: Origin) extends CTypeSpecifier with CDoubleImpl
case class CSigned()(implicit val o: Origin) extends CTypeSpecifier with CSignedImpl
case class CUnsigned()(implicit val o: Origin) extends CTypeSpecifier with CUnsignedImpl
case class CBool()(implicit val o: Origin) extends CTypeSpecifier with CBoolImpl
case class CTypedefName(name: String)(implicit val o: Origin) extends CTypeSpecifier with CTypedefNameImpl {
  var ref: Option[CTypeNameTarget] = None
}
case class CSpecificationType(t: Type)(implicit val o: Origin) extends CTypeSpecifier with CSpecificationTypeImpl

case class CTypeQualifierDeclarationSpecifier(typeQual: CTypeQualifier)(implicit val o: Origin) extends CDeclarationSpecifier with CTypeQualifierDeclarationSpecifierImpl

sealed trait CTypeQualifier extends NodeFamily with CTypeQualifierImpl
case class CConst()(implicit val o: Origin) extends CTypeQualifier with CConstImpl
case class CRestrict()(implicit val o: Origin) extends CTypeQualifier with CRestrictImpl
case class CVolatile()(implicit val o: Origin) extends CTypeQualifier with CVolatileImpl
case class CAtomic()(implicit val o: Origin) extends CTypeQualifier with CAtomicImpl

sealed trait CFunctionSpecifier extends CDeclarationSpecifier with CFunctionSpecifierImpl
sealed trait CAlignmentSpecifier extends CDeclarationSpecifier with CAlignmentSpecifierImpl

sealed trait CGpgpuKernelSpecifier extends CDeclarationSpecifier with CGpgpuKernelSpecifierImpl
case class CKernel()(implicit val o: Origin) extends CGpgpuKernelSpecifier with CKernelImpl

case class CPointer(qualifiers: Seq[CTypeQualifier])(implicit val o: Origin) extends NodeFamily with CPointerImpl

class CParam(val specifiers: Seq[CDeclarationSpecifier], val declarator: CDeclarator)(implicit val o: Origin) extends Declaration with CParamImpl

sealed trait CDeclarator extends NodeFamily with CDeclaratorImpl
case class CPointerDeclarator(pointers: Seq[CPointer], inner: CDeclarator)(implicit val o: Origin) extends CDeclarator with CPointerDeclaratorImpl
case class CArrayDeclarator(qualifiers: Seq[CTypeQualifier], size: Option[Expr], inner: CDeclarator)(implicit val o: Origin) extends CDeclarator with CArrayDeclaratorImpl
case class CTypedFunctionDeclarator(params: Seq[CParam], varargs: Boolean, inner: CDeclarator)(implicit val o: Origin) extends CDeclarator with CTypedFunctionDeclaratorImpl
case class CAnonymousFunctionDeclarator(params: Seq[String], inner: CDeclarator)(implicit val o: Origin) extends CDeclarator with CAnonymousFunctionDeclaratorImpl
case class CName(name: String)(implicit val o: Origin) extends CDeclarator with CNameImpl

case class CInit(decl: CDeclarator, init: Option[Expr])(implicit val o: Origin) extends NodeFamily with CInitImpl

class CDeclaration(val contract: ApplicableContract, val kernelInvariant: Expr, val specs: Seq[CDeclarationSpecifier], val inits: Seq[CInit])(implicit val o: Origin) extends Declaration with CDeclarationImpl

sealed trait CAbstractGlobalDeclaration extends GlobalDeclaration with CAbstractGlobalDeclarationImpl

class CFunctionDefinition(val specs: Seq[CDeclarationSpecifier], val declarator: CDeclarator, val body: Statement)(val blame: Blame[PostconditionFailed])(implicit val o: Origin) extends CAbstractGlobalDeclaration with CFunctionDefinitionImpl

class CGlobalDeclaration(val decl: CDeclaration)(implicit val o: Origin) extends CAbstractGlobalDeclaration with CGlobalDeclarationImpl

sealed trait CStatement extends Statement with CStatementImpl
case class CDeclarationStatement(decl: CDeclaration)(implicit val o: Origin) extends CStatement with CDeclarationStatementImpl
case class CGoto(label: String)(implicit val o: Origin) extends CStatement with CGotoImpl {
  var ref: Option[LabelDecl] = None
}

case class GpgpuLocalBarrier(requires: Expr, ensures: Expr)(implicit val o: Origin) extends CStatement with GpgpuLocalBarrierImpl
case class GpgpuGlobalBarrier(requires: Expr, ensures: Expr)(implicit val o: Origin) extends CStatement with GpgpuGlobalBarrierImpl
case class GpgpuAtomic(impl: Statement, before: Statement, after: Statement)(implicit val o: Origin) extends CStatement with GpgpuAtomicImpl

sealed trait CExpr extends Expr with CExprImpl
case class CLocal(name: String)(implicit val o: Origin) extends CExpr with CLocalImpl {
  var ref: Option[CNameTarget] = None
}
case class CInvocation(applicable: Expr, args: Seq[Expr], givenArgs: Seq[(String, Expr)], yields: Seq[(Expr, String)])(implicit val o: Origin) extends CExpr with CInvocationImpl {
  var ref: Option[CInvocationTarget] = None
}
case class CStructAccess(struct: Expr, field: String)(val blame: Blame[FrontendDerefError])(implicit val o: Origin) extends CExpr with CStructAccessImpl {
  var ref: Option[CDerefTarget] = None
}
case class CStructDeref(struct: Expr, field: String)(implicit val o: Origin) extends CExpr with CStructDerefImpl
case class GpgpuCudaKernelInvocation(kernel: String, blocks: Expr, threads: Expr, args: Seq[Expr], givenArgs: Seq[(String, Expr)], yields: Seq[(Expr, String)])(implicit val o: Origin) extends CExpr with GpgpuCudaKernelInvocationImpl {
  var ref: Option[CInvocationTarget] = None
}

sealed trait CType extends Type with CTypeImpl
case class CPrimitiveType(specifiers: Seq[CDeclarationSpecifier])(implicit val o: Origin = DiagnosticOrigin) extends CType with CPrimitiveTypeImpl

case class JavaName(names: Seq[String])(implicit val o: Origin) extends NodeFamily with JavaNameImpl {
  var ref: Option[JavaTypeNameTarget] = None
}
case class JavaImport(isStatic: Boolean, name: JavaName, star: Boolean)(implicit val o: Origin) extends NodeFamily with JavaImportImpl

sealed trait JavaModifier extends NodeFamily with JavaModifierImpl
case class JavaPublic()(implicit val o: Origin) extends JavaModifier with JavaPublicImpl
case class JavaProtected()(implicit val o: Origin) extends JavaModifier with JavaProtectedImpl
case class JavaPrivate()(implicit val o: Origin) extends JavaModifier with JavaPrivateImpl
case class JavaStatic()(implicit val o: Origin) extends JavaModifier with JavaStaticImpl
case class JavaAbstract()(implicit val o: Origin) extends JavaModifier with JavaAbstractImpl
case class JavaFinal()(implicit val o: Origin) extends JavaModifier with JavaFinalImpl
case class JavaStrictFP()(implicit val o: Origin) extends JavaModifier with JavaStrictFPImpl
case class JavaNative()(implicit val o: Origin) extends JavaModifier with JavaNativeImpl
case class JavaSynchronized()(implicit val o: Origin) extends JavaModifier with JavaSynchronizedImpl
case class JavaTransient()(implicit val o: Origin) extends JavaModifier with JavaTransientImpl
case class JavaVolatile()(implicit val o: Origin) extends JavaModifier with JavaVolatileImpl

case class JavaPure()(implicit val o: Origin) extends JavaModifier with JavaPureImpl
case class JavaInline()(implicit val o: Origin) extends JavaModifier with JavaInlineImpl

sealed trait JavaGlobalDeclaration extends GlobalDeclaration with JavaGlobalDeclarationImpl
class JavaNamespace(val pkg: Option[JavaName], val imports: Seq[JavaImport], val declarations: Seq[GlobalDeclaration])(implicit val o: Origin) extends JavaGlobalDeclaration with Declarator with JavaNamespaceImpl

sealed abstract class JavaClassOrInterface extends JavaGlobalDeclaration with Declarator with JavaClassOrInterfaceImpl
class JavaClass(val name: String, val modifiers: Seq[JavaModifier], val typeParams: Seq[Variable], val intrinsicLockInvariant: Expr, val ext: Type, val imp: Seq[Type], val decls: Seq[ClassDeclaration])(implicit val o: Origin) extends JavaClassOrInterface with JavaClassImpl
class JavaInterface(val name: String, val modifiers: Seq[JavaModifier], val typeParams: Seq[Variable], val ext: Seq[Type], val decls: Seq[ClassDeclaration])(implicit val o: Origin) extends JavaClassOrInterface with JavaInterfaceImpl

sealed trait JavaClassDeclaration extends ClassDeclaration with JavaClassDeclarationImpl
class JavaSharedInitialization(val isStatic: Boolean, val initialization: Statement)(implicit val o: Origin) extends JavaClassDeclaration with JavaSharedInitializationImpl
class JavaFields(val modifiers: Seq[JavaModifier], val t: Type, val decls: Seq[(String, Int, Option[Expr])])(implicit val o: Origin) extends JavaClassDeclaration with JavaFieldsImpl
class JavaConstructor(val modifiers: Seq[JavaModifier], val name: String, val parameters: Seq[Variable], val typeParameters: Seq[Variable], val signals: Seq[JavaName], val body: Statement, val contract: ApplicableContract)(implicit val o: Origin) extends JavaClassDeclaration with JavaConstructorImpl
class JavaMethod(val modifiers: Seq[JavaModifier], val returnType: Type, val dims: Int, val name: String, val parameters: Seq[Variable], val typeParameters: Seq[Variable], val signals: Seq[JavaName], val body: Option[Statement], val contract: ApplicableContract)(val blame: Blame[PostconditionFailed])(implicit val o: Origin) extends JavaClassDeclaration with JavaMethodImpl

class JavaLocalDeclaration(val modifiers: Seq[JavaModifier], val t: Type, val decls: Seq[(String, Int, Option[Expr])])(implicit val o: Origin) extends Declaration with JavaLocalDeclarationImpl

sealed trait JavaStatement extends Statement with JavaStatementImpl
case class JavaLocalDeclarationStatement(decl: JavaLocalDeclaration)(implicit val o: Origin) extends JavaStatement with JavaLocalDeclarationStatementImpl

sealed trait JavaType extends Type with JavaTypeImpl
case class JavaNamedType(names: Seq[(String, Option[Seq[Type]])])(implicit val o: Origin) extends JavaType with JavaNamedTypeImpl {
  var ref: Option[JavaTypeNameTarget] = None
}
case class JavaTClass(ref: Ref[JavaClassOrInterface], typeArgs: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends JavaType with JavaTClassImpl

sealed trait JavaExpr extends Expr with JavaExprImpl
case class JavaLocal(name: String)(val blame: Blame[DerefInsufficientPermission])(implicit val o: Origin) extends JavaExpr with JavaLocalImpl {
  var ref: Option[JavaNameTarget] = None
}
case class JavaDeref(obj: Expr, field: String)(val blame: Blame[FrontendDerefError])(implicit val o: Origin) extends JavaExpr with JavaDerefImpl {
  var ref: Option[JavaDerefTarget] = None
}
case class JavaLiteralArray(exprs: Seq[Expr])(implicit val o: Origin) extends JavaExpr with JavaLiteralArrayImpl {
  var typeContext: Option[Type] = None
}
case class JavaInvocation(obj: Option[Expr], typeParams: Seq[Type], method: String, arguments: Seq[Expr], givenArgs: Seq[(String, Expr)], yields: Seq[(Expr, String)])(val blame: Blame[FrontendInvocationError])(implicit val o: Origin) extends JavaExpr with JavaInvocationImpl {
  var ref: Option[JavaInvocationTarget] = None
}
case class JavaNewClass(args: Seq[Expr], typeArgs: Seq[Type], name: Type)(val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends JavaExpr with JavaNewClassImpl
case class JavaNewLiteralArray(baseType: Type, dims: Int, initializer: Expr)(implicit val o: Origin) extends JavaExpr with JavaNewLiteralArrayImpl
case class JavaNewDefaultArray(baseType: Type, specifiedDims: Seq[Expr], moreDims: Int)(implicit val o: Origin) extends JavaExpr with JavaNewDefaultArrayImpl

sealed trait PVLType extends Type with PVLTypeImpl
case class PVLNamedType(name: String, typeArgs: Seq[Type])(implicit val o: Origin = DiagnosticOrigin) extends PVLType with PVLNamedTypeImpl {
  var ref: Option[PVLTypeNameTarget] = None
}

sealed trait PVLExpr extends Expr with PVLExprImpl
case class PVLLocal(name: String)(val blame: Blame[DerefInsufficientPermission])(implicit val o: Origin) extends PVLExpr with PVLLocalImpl {
  var ref: Option[PVLNameTarget] = None
}
case class PVLDeref(obj: Expr, field: String)(val blame: Blame[FrontendDerefError])(implicit val o: Origin) extends PVLExpr with PVLDerefImpl {
  var ref: Option[PVLDerefTarget] = None
}
case class PVLInvocation(obj: Option[Expr], method: String, args: Seq[Expr], typeArgs: Seq[Type], givenArgs: Seq[(String, Expr)], yields: Seq[(Expr, String)])(val blame: Blame[FrontendInvocationError])(implicit val o: Origin) extends PVLExpr with PVLInvocationImpl {
  var ref: Option[PVLInvocationTarget] = None
}

case class PVLNew(t: Type, args: Seq[Expr])(val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends PVLExpr with PVLNewImpl

sealed trait PVLClassDeclaration extends ClassDeclaration with PVLClassDeclarationImpl
class PVLConstructor(val contract: ApplicableContract, val args: Seq[Variable], val body: Option[Statement])(implicit val o: Origin) extends PVLClassDeclaration with PVLConstructorImpl

case class SilverPredicateAccess(ref: Ref[Predicate], args: Seq[Expr], perm: Expr)(implicit val o: Origin) extends NodeFamily with SilverPredicateAccessImpl

sealed trait SilverExpr extends Expr with SilverExprImpl
case class SilverDeref(obj: Expr, field: Ref[SilverField])(val blame: Blame[InsufficientPermission])(implicit val o: Origin) extends SilverExpr with HeapDeref with SilverDerefImpl

sealed trait SilverResource extends SilverExpr with SilverResourceImpl
case class SilverPerm(obj: Expr, field: Ref[SilverField], perm: Expr)(implicit val o: Origin) extends SilverResource with SilverPermImpl
case class SilverPredPerm(access: SilverPredicateAccess)(implicit val o: Origin) extends SilverResource with SilverPredPermImpl

case class SilverUnfolding(access: SilverPredicateAccess, body: Expr)(implicit val o: Origin) extends SilverExpr with SilverUnfoldingImpl
case class SilverCurFieldPerm(obj: Expr, field: Ref[SilverField])(implicit val o: Origin) extends SilverExpr with SilverCurFieldPermImpl
case class SilverCurPredPerm(ref: Ref[Predicate], args: Seq[Expr])(implicit val o: Origin) extends SilverExpr with SilverCurPredPermImpl

sealed trait SilverStatement extends Statement with SilverStatementImpl
case class SilverUnfold(access: SilverPredicateAccess)(val blame: Blame[SilverUnfoldFailed])(implicit val o: Origin) extends SilverStatement with SilverUnfoldImpl
case class SilverFold(access: SilverPredicateAccess)(val blame: Blame[SilverFoldFailed])(implicit val o: Origin) extends SilverStatement with SilverFoldImpl
case class SilverWhile(cond: Expr, invariant: Expr, body: Statement)(val blame: Blame[SilverWhileInvariantFailure])(implicit val o: Origin) extends SilverStatement with SilverWhileImpl
case class SilverIf(cond: Expr, whenTrue: Statement, whenFalse: Statement)(implicit val o: Origin) extends SilverStatement with SilverIfImpl
case class SilverNewRef(v: Ref[Variable], fields: Seq[Ref[SilverField]])(implicit val o: Origin) extends SilverStatement with SilverNewRefImpl

sealed trait SilverAssign extends SilverStatement with SilverAssignImpl
case class SilverFieldAssign(obj: Expr, field: Ref[SilverField], value: Expr)(val blame: Blame[SilverAssignFailed])(implicit val o: Origin) extends SilverAssign with SilverFieldAssignImpl
case class SilverLocalAssign(v: Ref[Variable], value: Expr)(implicit val o: Origin) extends SilverAssign with SilverLocalAssignImpl

sealed abstract class SilverDeclaration extends GlobalDeclaration with SilverDeclarationImpl
class SilverField(val t: Type)(implicit val o: Origin) extends SilverDeclaration with SilverFieldImpl
