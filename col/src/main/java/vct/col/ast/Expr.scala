package vct.col.ast

import hre.util.FuncTools
import vct.col
import vct.col.check.{CheckContext, CheckError, TupleTypeCount, TypeError, TypeErrorText, UnreachableAfterTypeCheck}
import vct.col.coerce.Coercion
import vct.col.resolve.{RefAxiomaticDataType, RefJavaClass, RefModel, SpecTypeNameTarget}
import vct.col.origin._

sealed trait Expr extends NodeFamily {
  def checkSubType(other: Type): Seq[CheckError] =
    Coercion.getCoercion(t, other) match {
      case Some(_) => Nil
      case None => Seq(TypeError(this, other))
    }

  def t: Type

  private def unfold(node: Expr)(matchFunc: PartialFunction[Expr, Seq[Expr]]): Seq[Expr] =
    matchFunc.lift(node) match {
      case Some(value) => value.flatMap(unfold(_)(matchFunc))
      case None => Seq(node)
    }

  def unfoldStar: Seq[Expr] = unfold(this) { case Star(left, right) => Seq(left, right) }
  def unfoldProcessPar: Seq[Expr] = unfold(this) { case ProcessPar(l, r) => Seq(l, r) }
}

trait ExtraExpr extends Expr

sealed trait IntExpr extends Expr {
  override def t: Type = TInt()
}
sealed trait BoolExpr extends Expr {
  override def t: Type = TBool()
}
sealed trait ResourceExpr extends Expr {
  override def t: Type = TResource()
}
sealed trait RationalExpr extends Expr {
  override def t: Type = TRational()
}
sealed trait ProcessExpr extends Expr {
  override def t: Type = TProcess()
}
sealed trait VoidExpr extends Expr {
  override def t: Type = TVoid()
}

sealed abstract class Constant[T] extends Expr {
  def value: T

  override def equals(obj: scala.Any): Boolean = obj match {
    case const: Constant[T] => this.value == const.value
  }

  override def hashCode(): Int = value.hashCode()
  override def toString: String = value.toString
}

object Constant {
  def unapply(obj: scala.Any): Option[scala.Any] = obj match {
    case c: Constant[_] => Some(c.value)
    case _ => None
  }

  implicit def integer(value: Int)(implicit o: Origin): IntegerValue =
    new IntegerValue(value)

  // TODO: the code generation doesnt see IntegerValue <: Constant[_] <: Expr, hence the Expr marker here.
  implicit class IntegerValue(val value: BigInt)(implicit val o: Origin) extends Constant[BigInt] with Expr {
    override def t: Type = TBoundedInt(value, value + 1)
  }
  implicit class BooleanValue(val value: Boolean)(implicit val o: Origin) extends Constant[Boolean] with BoolExpr
}

case class LiteralSeq(element: Type, values: Seq[Expr])(implicit val o: Origin) extends Expr {
  override def t: Type = TSeq(element)
}

/* We do *not* want to statically deduplicate structurally equal expressions, since expressions may have side effects. */
case class LiteralSet(element: Type, values: Seq[Expr])(implicit val o: Origin) extends Expr {
  override def t: Type = TSet(element)
}

case class LiteralBag(element: Type, values: Seq[Expr])(implicit val o: Origin) extends Expr {
  override def t: Type = TBag(element)
}

case class LiteralTuple(ts: Seq[Type], values: Seq[Expr])(implicit val o: Origin) extends Expr {
  override def t: Type = TTuple(ts)

  override def check(context: CheckContext): Seq[CheckError] =
    if(ts.size == values.size) {
      super.check(context)
    } else {
      Seq(TupleTypeCount(this))
    }
}

case class LiteralMap(k: Type, v: Type, values: Seq[(Expr, Expr)])(implicit val o: Origin) extends Expr {
  override def t: Type = TMap(k, v)
}

case class UntypedLiteralSeq(values: Seq[Expr])(implicit val o: Origin) extends Expr {
  def elementType: Type = Type.leastCommonSuperType(values.map(_.t))
  override def t: Type = TSeq(elementType)
}

case class UntypedLiteralSet(values: Seq[Expr])(implicit val o: Origin) extends Expr {
  def elementType: Type = Type.leastCommonSuperType(values.map(_.t))
  override def t: Type = TSet(elementType)
}

case class UntypedLiteralBag(values: Seq[Expr])(implicit val o: Origin) extends Expr {
  def elementType: Type = Type.leastCommonSuperType(values.map(_.t))
  override def t: Type = TBag(elementType)
}

case class Void()(implicit val o: Origin) extends Expr {
  override def t: Type = TVoid()
}

case class ContextSensitiveNodeNotResolved(expr: Expr, message: String) extends ASTStateError {
  override def text: String =
    "A node was encountered of which the type is context-sensitive, but its context is not yet resolved. " +
    f"The node says: $message"
}

case class AmbiguousThis()(implicit val o: Origin) extends Expr {
  var ref: Option[Type] = None
  override def t: Type = ref.getOrElse(throw ContextSensitiveNodeNotResolved(this,
    "'this' encountered, but the surrounding class is not resolved."))
}

case class AmbiguousResult()(implicit val o: Origin) extends Expr {
  var ref: Option[Type] = None
  override def t: Type = ref.getOrElse(
    throw ContextSensitiveNodeNotResolved(this, "'\\result' encountered, but its attached method is not resolved."))
}

case class CurrentThreadId()(implicit val o: Origin) extends Expr {
  override def t: Type = TInt()
}

case class Null()(implicit val o: Origin) extends Expr {
  override def t: Type = TNull()
}

// The * in Perm(a[*], write)
case class Any()(implicit val o: Origin) extends Expr {
  override def t: Type = TInt()
}

case class NoPerm()(implicit val o: Origin) extends Expr {
  override def t: Type = TBoundedInt(0, 1)
}
case class ReadPerm()(implicit val o: Origin) extends Expr {
  override def t: Type = TFraction()
}
case class WritePerm()(implicit val o: Origin) extends Expr {
  override def t: Type = TBoundedInt(1, 2)
}

case class Range(from: Expr, to: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = TSeq(TInt())
}
case class Values(arr: Expr, from: Expr, to: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = TSeq(arr.t.asArray.get.element)
}

case class OptSome(e: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = TOption(e.t)
}
case class OptNone()(implicit val o: Origin) extends Expr {
  override def t: Type = TOption(TAny())
}

case class MapCons(map: Expr, k: Expr, v: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = tailType
  def tailType: TMap = map.t.asMap.get
}
case class MapEq(left: Expr, right: Expr)(implicit val o: Origin) extends BoolExpr
case class MapDisjoint(left: Expr, right: Expr)(implicit val o: Origin) extends BoolExpr
sealed trait MapOp extends Expr {
  def map: Expr
  def mapType: TMap = map.t.asMap.get
}
case class MapKeySet(map: Expr)(implicit val o: Origin) extends MapOp {
  override def t: Type = TSet(mapType.key)
}
case class MapValueSet(map: Expr)(implicit val o: Origin) extends MapOp {
  override def t: Type = TSet(mapType.value)
}
case class MapItemSet(map: Expr)(implicit val o: Origin) extends MapOp {
  override def t: Type = TSet(TTuple(Seq(mapType.key, mapType.value)))
}
case class MapSize(map: Expr)(implicit val o: Origin) extends MapOp with IntExpr
case class MapRemove(map: Expr, k: Expr)(implicit val o: Origin) extends Expr {
  def mapType: TMap = map.t.asMap.get
  override def t: Type = mapType
}

sealed trait Binder extends Declarator {
  def bindings: Seq[Variable]
  override def declarations: Seq[Declaration] = bindings
}

case class Forall(bindings: Seq[Variable], triggers: Seq[Seq[Expr]], body: Expr)(implicit val o: Origin) extends BoolExpr with Binder
case class Starall(bindings: Seq[Variable], triggers: Seq[Seq[Expr]], body: Expr)(implicit val o: Origin) extends ResourceExpr with Binder
case class Exists(bindings: Seq[Variable], triggers: Seq[Seq[Expr]], body: Expr)(implicit val o: Origin) extends BoolExpr with Binder
case class Sum(bindings: Seq[Variable], condition: Expr, main: Expr)(implicit val o: Origin) extends IntExpr with Binder
case class Product(bindings: Seq[Variable], condition: Expr, main: Expr)(implicit val o: Origin) extends IntExpr with Binder
case class Let(binding: Variable, value: Expr, main: Expr)(implicit val o: Origin) extends Expr with Binder {
  override def t: Type = main.t
  override def bindings: Seq[Variable] = Seq(binding)
}
case class InlinePattern(inner: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = inner.t
}

case class Local(ref: Ref[Variable])(implicit val o: Origin) extends Expr {
  override def t: Type = ref.decl.t
  override def check(context: CheckContext): Seq[CheckError] =
    context.checkInScope(this, ref)
}
trait HeapDeref
case class Deref(obj: Expr, ref: Ref[InstanceField])(val blame: Blame[InsufficientPermission])(implicit val o: Origin) extends Expr with HeapDeref {
  override def t: Type = ref.decl.t
  override def check(context: CheckContext): Seq[CheckError] =
    context.checkInScope(this, ref)
}
case class ModelDeref(obj: Expr, ref: Ref[ModelField])(val blame: Blame[ModelInsufficientPermission])(implicit val o: Origin) extends Expr {
  override def t: Type = ref.decl.t
  override def check(context: CheckContext): Seq[CheckError] =
    context.checkInScope(this, ref)
}
case class DerefPointer(pointer: Expr)(val blame: Blame[PointerDerefError])(implicit val o: Origin) extends Expr {
  override def t: Type = pointer.t.asPointer.get.element
}
case class AddrOf(e: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = TPointer(e.t)
}

sealed trait Apply extends Expr {
  def ref: Ref[_ <: Applicable]
  def args: Seq[Expr]

  override def t: Type = ref.decl.returnType
}

case class PredicateApply(ref: Ref[Predicate], args: Seq[Expr])(implicit val o: Origin) extends Apply
case class InstancePredicateApply(obj: Expr, ref: Ref[InstancePredicate], args: Seq[Expr])(implicit val o: Origin) extends Apply

case class ADTFunctionInvocation(typeArgs: Option[(Ref[AxiomaticDataType], Seq[Type])],
                                 ref: Ref[ADTFunction], args: Seq[Expr])(implicit val o: Origin) extends Apply {
  override def t: Type =
    typeArgs match {
      case Some((adt, typeArgs)) =>
        ref.decl.returnType.particularize(adt.decl.typeArgs.zip(typeArgs).toMap)
      case None => ref.decl.returnType
    }
}

sealed trait Invocation extends Apply {
  override def ref: Ref[_ <: ContractApplicable]
  def blame: Blame[PreconditionFailed]
  def typeArgs: Seq[Type]

  override def t: Type = super.t.particularize(ref.decl.typeArgs.zip(typeArgs).toMap)
}

case class ProcedureInvocation(ref: Ref[Procedure], args: Seq[Expr], outArgs: Seq[Ref[Variable]], typeArgs: Seq[Type])
                              (val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends Invocation
case class FunctionInvocation(ref: Ref[Function], args: Seq[Expr], typeArgs: Seq[Type])
                             (val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends Invocation

case class MethodInvocation(obj: Expr, ref: Ref[InstanceMethod], args: Seq[Expr], outArgs: Seq[Ref[Variable]], typeArgs: Seq[Type])
                           (val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends Invocation
case class InstanceFunctionInvocation(obj: Expr, ref: Ref[InstanceFunction], args: Seq[Expr], typeArgs: Seq[Type])
                                     (val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends Invocation

sealed trait UnExpr extends Expr {
  def arg: Expr
}

case class UMinus(arg: Expr)(implicit val o: Origin) extends UnExpr {
  override def t: Type =
    Coercion.getCoercion(arg.t, TInt()) match {
      case Some(_) => TInt()
      case _ => TRational()
    }
}
case class BitNot(arg: Expr)(implicit val o: Origin) extends UnExpr with IntExpr
case class Not(arg: Expr)(implicit val o: Origin) extends UnExpr with BoolExpr

sealed trait BinExpr extends Expr {
  def left: Expr
  def right: Expr
}
sealed trait NumericBinExpr extends BinExpr {
  def isIntOp: Boolean =
    Coercion.getCoercion(left.t, TInt()).isDefined &&
      Coercion.getCoercion(right.t, TInt()).isDefined

  override def t: Type = if(isIntOp) TInt() else TRational()
}

sealed trait IntBinExpr extends BinExpr {
  override def t: Type = TInt()
}

sealed trait BoolBinExpr extends BinExpr {
  override def t: Type = TBool()
}

sealed trait DividingExpr extends Expr {
  def blame: Blame[DivByZero]
}

case class AmbiguousMult(left: Expr, right: Expr)(implicit val o: Origin) extends Expr {
  def isProcessOp: Boolean = Coercion.getCoercion(left.t, TProcess()).isDefined
  def isIntOp: Boolean = Coercion.getCoercion(left.t, TInt()).isDefined && Coercion.getCoercion(right.t, TInt()).isDefined

  override def t: Type = if(isProcessOp) TProcess() else (if(isIntOp) TInt() else TRational())
}

case class AmbiguousPlus(left: Expr, right: Expr)(implicit val o: Origin) extends Expr {
  def isProcessOp: Boolean = Coercion.getCoercion(left.t, TProcess()).isDefined
  def isSeqOp: Boolean = Coercion.getAnySeqCoercion(left.t).isDefined
  def isBagOp: Boolean = Coercion.getAnyBagCoercion(left.t).isDefined
  def isSetOp: Boolean = Coercion.getAnySetCoercion(left.t).isDefined
  def isPointerOp: Boolean = Coercion.getAnyPointerCoercion(left.t).isDefined
  def isIntOp: Boolean =
    Coercion.getCoercion(left.t, TInt()).isDefined &&
      Coercion.getCoercion(right.t, TInt()).isDefined

  override def t: Type =
    if(isProcessOp) TProcess()
    else if(isSeqOp || isBagOp || isSetOp) Type.leastCommonSuperType(left.t, right.t)
    else if(isPointerOp) left.t
    else if(isIntOp) TInt()
    else TRational()
}


case class AmbiguousOr(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr {
  def isProcessOp: Boolean = Coercion.getCoercion(left.t, TProcess()).isDefined

  override def t: Type = if(isProcessOp) TProcess() else TBool()
}

sealed trait BitOp extends BinExpr {
  def isBoolOp: Boolean = Coercion.getCoercion(left.t, TBool()).isDefined

  override def t: Type = if(isBoolOp) TBool() else TInt()
}

case class AmbiguousComputationalOr(left: Expr, right: Expr)(implicit val o: Origin) extends BitOp
case class AmbiguousComputationalXor(left: Expr, right: Expr)(implicit val o: Origin) extends BitOp
case class AmbiguousComputationalAnd(left: Expr, right: Expr)(implicit val o: Origin) extends BitOp

case class Exp(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr
case class Plus(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr
case class Minus(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr
case class Mult(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr
case class Div(left: Expr, right: Expr)(val blame: Blame[DivByZero])(implicit val o: Origin) extends BinExpr with RationalExpr with DividingExpr
case class FloorDiv(left: Expr, right: Expr)(val blame: Blame[DivByZero])(implicit val o: Origin) extends IntBinExpr with DividingExpr
case class Mod(left: Expr, right: Expr)(val blame: Blame[DivByZero])(implicit val o: Origin) extends NumericBinExpr with DividingExpr

case class BitAnd(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr
case class BitOr(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr
case class BitXor(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr
case class BitShl(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr
// sign-extended (signed)
case class BitShr(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr
// not sign-extended (unsigned)
case class BitUShr(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr

object And {
  def fold(exprs: Seq[Expr])(implicit o: Origin): Expr =
    exprs.reduceOption(And(_, _)).getOrElse(Constant.BooleanValue(true))
}

case class And(left: Expr, right: Expr)(implicit val o: Origin) extends BoolBinExpr
case class Or(left: Expr, right: Expr)(implicit val o: Origin) extends BoolBinExpr

object Implies {
  def unfold(expr: Expr): (Seq[Expr], Expr) = expr match {
    case Implies(left, right) =>
      val (antecedent, consequent) = unfold(right)
      (Star.unfold(left) ++ antecedent, consequent)
    case other => (Nil, other)
  }
}

case class Implies(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr {
  override def t: Type = right.t
}

object Star {
  def fold(exprs: Seq[Expr])(implicit o: Origin): Expr =
    exprs.reduceOption(Star(_, _)).getOrElse(Constant.BooleanValue(true))

  def unfold(expr: Expr): Seq[Expr] = expr match {
    case Star(left, right) => unfold(left) ++ unfold(right)
    case And(left, right) => unfold(left) ++ unfold(right)
    case Constant(true) => Nil
    case other => Seq(other)
  }
}

case class Star(left: Expr, right: Expr)(implicit val o: Origin) extends ResourceExpr
case class Wand(left: Expr, right: Expr)(implicit val o: Origin) extends ResourceExpr
case class Scale(scale: Expr, res: Expr)(implicit val o: Origin) extends ResourceExpr

case class Unfolding(pred: Expr, body: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = body.t
}

case class Perm(loc: Expr, perm: Expr)(implicit val o: Origin) extends ResourceExpr
case class HPerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends ResourceExpr
case class APerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends ResourceExpr
case class PointsTo(loc: Expr, perm: Expr, value: Expr)(implicit val o: Origin) extends ResourceExpr

case class CurPerm(loc: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = TRational()
}

case class ValidArray(arr: Expr, len: Expr)(implicit val o: Origin) extends BoolExpr
case class ValidMatrix(mat: Expr, w: Expr, h: Expr)(implicit val o: Origin) extends BoolExpr

case class PermPointer(p: Expr, len: Expr, perm: Expr)(implicit val o: Origin) extends ResourceExpr
case class PermPointerIndex(p: Expr, idx: Expr, perm: Expr)(implicit val o: Origin) extends ResourceExpr

sealed trait Comparison extends BinExpr {
  override def t: Type = TBool()
  def comparisonType: Type = Type.leastCommonSuperType(left.t, right.t)
}

case class Eq(left: Expr, right: Expr)(implicit val o: Origin) extends Comparison
case class Neq(left: Expr, right: Expr)(implicit val o: Origin) extends Comparison

sealed trait OrderOp extends Comparison {
  def isSetOp: Boolean = Coercion.getAnySetCoercion(left.t).isDefined

  def isBagOp: Boolean = Coercion.getAnyBagCoercion(left.t).isDefined

  def isIntOp: Boolean =
    Coercion.getCoercion(left.t, TInt()).isDefined &&
      Coercion.getCoercion(right.t, TInt()).isDefined
}

case class Greater(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp
case class Less(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp
case class GreaterEq(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp
case class LessEq(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp

case class Select(condition: Expr, whenTrue: Expr, whenFalse: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type =
    Type.leastCommonSuperType(whenTrue.t, whenFalse.t)
}

case class NewObject(cls: Ref[Class])(implicit val o: Origin) extends Expr {
  override def t: Type = TClass(cls)
}

case class NewArray(element: Type, dims: Seq[Expr], moreDims: Int)(implicit val o: Origin) extends Expr {
  override def t: Type = FuncTools.repeat(TArray(_), dims.size + moreDims, element)
}

case class Old(expr: Expr, at: Option[Ref[LabelDecl]])(val blame: Blame[LabelNotReached])(implicit val o: Origin) extends Expr {
  override def t: Type = expr.t
}

case class AmbiguousSubscript(collection: Expr, index: Expr)(implicit val o: Origin) extends Expr {
  def isSeqOp: Boolean = Coercion.getAnySeqCoercion(collection.t).isDefined

  def isArrayOp: Boolean = Coercion.getAnyArrayCoercion(collection.t).isDefined

  def isPointerOp: Boolean = Coercion.getAnyPointerCoercion(collection.t).isDefined

  def isMapOp: Boolean = Coercion.getAnyMapCoercion(collection.t).isDefined

  override def t: Type =
    if (isSeqOp) collection.t.asSeq.get.element
    else if (isArrayOp) collection.t.asArray.get.element
    else if (isPointerOp) collection.t.asPointer.get.element
    else collection.t.asMap.get.value
}

case class SeqSubscript(seq: Expr, index: Expr)(val blame: Blame[SeqBoundFailure])(implicit val o: Origin) extends Expr {
  override def t: Type = seq.t.asSeq.get.element
}

case class ArraySubscript(arr: Expr, index: Expr)(val blame: Blame[ArraySubscriptError])(implicit val o: Origin) extends Expr {
  override def t: Type = arr.t.asArray.get.element
}

case class PointerSubscript(pointer: Expr, index: Expr)(val blame: Blame[PointerSubscriptError])(implicit val o: Origin) extends Expr {
  override def t: Type = pointer.t.asPointer.get.element
}

case class Length(arr: Expr)(val blame: Blame[ArrayNull])(implicit val o: Origin) extends IntExpr

case class Size(obj: Expr)(implicit val o: Origin) extends IntExpr {
  override def check(context: CheckContext): Seq[CheckError] =
    obj.t match {
      case _: CollectionType => Seq()
      case _ => Seq(TypeErrorText(obj, got => s"Expected a collection type such as a sequence or map, but got $got"))
    }
}

case class Cons(x: Expr, xs: Expr)(implicit val o: Origin) extends Expr {
  def tailType: TSeq = xs.t.asSeq.get

  override def t: TSeq = TSeq(Type.leastCommonSuperType(tailType.element, x.t))
}

case class Head(xs: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = xs.t.asSeq.get.element
}

case class Tail(xs: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = xs.t
}

case class Drop(xs: Expr, count: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = xs.t
}

case class Take(xs: Expr, count: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = xs.t
}

case class Slice(xs: Expr, from: Expr, to: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = xs.t
}

case class SeqUpdate(xs: Expr, i: Expr, x: Expr)(implicit val o: Origin) extends Expr {
  def tailType: TSeq = xs.t.asSeq.get

  override def t: TSeq = TSeq(Type.leastCommonSuperType(tailType.element, x.t))
}

case class Concat(xs: Expr, ys: Expr)(implicit val o: Origin) extends Expr {
  def leftType: TSeq = xs.t.asSeq.get

  def rightType: TSeq = ys.t.asSeq.get

  override def t: Type = TSeq(Type.leastCommonSuperType(leftType.element, rightType.element))
}

case class RemoveAt(xs: Expr, i: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = xs.t
}

case class Empty(obj: Expr)(implicit val o: Origin) extends IntExpr

case class AmbiguousMember(x: Expr, xs: Expr)(implicit val o: Origin) extends Expr {
  def isSeqOp: Boolean = Coercion.getAnySeqCoercion(xs.t).isDefined
  def isSetOp: Boolean = Coercion.getAnySetCoercion(xs.t).isDefined
  def isMapOp: Boolean = Coercion.getAnyMapCoercion(xs.t).isDefined
  def isBagOp: Boolean = Coercion.getAnyBagCoercion(xs.t).isDefined

  def collectionElementType: Type =
    if(isSeqOp) xs.t.asSeq.get.element
    else if(isSetOp) xs.t.asSet.get.element
    else if(isBagOp) xs.t.asBag.get.element
    else xs.t.asMap.get.key

  override def t: Type = if(isBagOp) TInt() else TBool()
}
case class SetMember(x: Expr, xs: Expr)(implicit val o: Origin) extends BoolExpr
case class SeqMember(x: Expr, xs: Expr)(implicit val o: Origin) extends BoolExpr
case class MapMember(x: Expr, xs: Expr)(implicit val o: Origin) extends BoolExpr
case class BagMemberCount(x: Expr, xs: Expr)(implicit val o: Origin) extends IntExpr

sealed trait SetComparison extends Comparison
case class SubSet(left: Expr, right: Expr)(implicit val o: Origin) extends SetComparison
case class SubSetEq(left: Expr, right: Expr)(implicit val o: Origin) extends SetComparison
case class Permutation(xs: Expr, ys: Expr)(implicit val o: Origin) extends BoolExpr
case class OptGet(opt: Expr)(val blame: Blame[OptionNone])(implicit val o: Origin) extends Expr {
  override def t: Type = opt.t.asOption.get.element
}
case class OptGetOrElse(opt: Expr, alt: Expr)(implicit val o: Origin) extends Expr {
  def optionType: TOption = opt.t.asOption.get
  override def t: Type = Type.leastCommonSuperType(optionType.element, alt.t)
}
case class MapGet(map: Expr, k: Expr)(val blame: Blame[MapKeyError])(implicit val o: Origin) extends Expr {
  def mapType: TMap = map.t.asMap.get
  override def t: Type = mapType.value
}
case class TupGet(tup: Expr, index: Int)(implicit val o: Origin) extends Expr {
  def tupleType: TTuple = tup.t.asTuple.get
  override def t: Type = tupleType.elements(index)
  override def check(context: CheckContext): Seq[CheckError] =
    super.check(context) match {
      case Nil => if(0 <= index && index < tupleType.elements.size) Nil else Seq(col.check.TypeErrorText(this, _ => "Tuple getter exceeds tuple size"))
      case some => some
    }
}

case class VectorSum(indices: Expr, vec: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = vec.t.asSeq.get.element
}
case class VectorCompare(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr {
  override def t: Type = TSeq(TInt()) // the results are 0 or 1, mimicking TSeq(TBool())
}
case class VectorRepeat(e: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = TSeq(e.t)
}
case class MatrixSum(indices: Expr, mat: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = mat.t.asMatrix.get.element
}
case class MatrixCompare(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr {
  override def t: Type = TMatrix(TInt())
}
case class MatrixRepeat(e: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = TMatrix(e.t)
}

case class TypeValue(value: Type)(implicit val o: Origin) extends Expr {
  override def t: Type = TType(value)
}
case class TypeOf(expr: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = TType(expr.t)
}
case class InstanceOf(value: Expr, typeValue: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = TBool()
}
case class Cast(value: Expr, typeValue: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = typeValue.t match {
    case TType(t) => t
    case _ => throw UnreachableAfterTypeCheck("The cast type is not a type", this)
  }
}

sealed trait TypeComparison extends Comparison
case class SubType(left: Expr, right: Expr)(implicit val o: Origin) extends TypeComparison
case class SuperType(left: Expr, right: Expr)(implicit val o: Origin) extends TypeComparison

sealed trait AssignExpression extends Expr {
  def target: Expr
  def value: Expr
}

case class PreAssignExpression(target: Expr, value: Expr)(implicit val o: Origin) extends AssignExpression {
  override def t: Type = value.t
}
case class PostAssignExpression(target: Expr, value: Expr)(implicit val o: Origin) extends AssignExpression {
  override def t: Type = target.t
}

case class With(pre: Statement, value: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = value.t
}
case class Then(value: Expr, post: Statement)(implicit val o: Origin) extends Expr {
  override def t: Type = value.t
}

case class Held(obj: Expr)(implicit val o: Origin) extends BoolExpr
case class IdleToken(thread: Expr)(implicit val o: Origin) extends BoolExpr
case class JoinToken(thread: Expr)(implicit val o: Origin) extends BoolExpr

case class EmptyProcess()(implicit val o: Origin) extends ProcessExpr
case class ActionApply(action: Ref[ModelAction], args: Seq[Expr])(implicit val o: Origin) extends ProcessExpr
case class ProcessApply(process: Ref[ModelProcess], args: Seq[Expr])(implicit val o: Origin) extends ProcessExpr
case class ProcessSeq(left: Expr, right: Expr)(implicit val o: Origin) extends ProcessExpr
case class ProcessChoice(left: Expr, right: Expr)(implicit val o: Origin) extends ProcessExpr
case class ProcessPar(left: Expr, right: Expr)(implicit val o: Origin) extends ProcessExpr
case class ProcessSelect(cond: Expr, whenTrue: Expr, whenFalse: Expr)(implicit val o: Origin) extends ProcessExpr

case class ModelNew(ref: Ref[Model])(implicit val o: Origin) extends Expr {
  override def t: Type = TModel(ref)
}

case class ModelState(model: Expr, perm: Expr, state: Expr)(implicit val o: Origin) extends ResourceExpr
case class ModelAbstractState(model: Expr, state: Expr)(implicit val o: Origin) extends ResourceExpr
case class ModelCreate(model: Expr, init: Expr)(implicit val o: Origin) extends VoidExpr
case class ModelDestroy(model: Expr)(implicit val o: Origin) extends VoidExpr
case class ModelSplit(model: Expr, leftPerm: Expr, leftProcess: Expr, rightPerm: Expr, rightProcess: Expr)(implicit val o: Origin) extends VoidExpr
case class ModelMerge(model: Expr, leftPerm: Expr, leftProcess: Expr, rightPerm: Expr, rightProcess: Expr)(implicit val o: Origin) extends VoidExpr
case class ModelChoose(model: Expr, perm: Expr, totalProcess: Expr, choice: Expr)(implicit val o: Origin) extends VoidExpr

case class ModelPerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends ResourceExpr
case class ActionPerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends ResourceExpr