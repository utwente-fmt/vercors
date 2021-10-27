package vct.col.ast

import hre.util.FuncTools
import vct.col.resolve.{RefAxiomaticDataType, RefJavaClass, RefModel, SpecTypeNameTarget}

sealed trait Expr extends NodeFamily {
  def t: Type

  def checkSubType(other: Type): Seq[CheckError] =
    if(other.superTypeOf(t))
      Nil
    else
      Seq(TypeError(this, other))

  private def isOfClassType: Boolean = t.mimics match {
    case TClass(_) => true
    case t @ JavaTClass(_) => t.ref.get match {
      case RefAxiomaticDataType(_) => false
      case RefModel(_) => false
      case RefJavaClass(_) => true
    }
    case _ => false
  }

  def checkClassType: Seq[CheckError] =
    if(isOfClassType) Nil
    else Seq(TypeErrorText(this, got => s"Expected a class type, but got $got"))

  def checkModelType: Seq[CheckError] =
    t match {
      case TModel(_) => Nil
      case _ => Seq(TypeErrorText(this, got => s"Expected a model type, but got $got"))
    }

  def checkSeqThen(whenOk: TSeq => Seq[CheckError] = _ => Nil): Seq[CheckError] =
    t.asSeq.map(whenOk).getOrElse(Seq(TypeError(this, TSeq(TAny()))))
  def checkSetThen(whenOk: TSet => Seq[CheckError] = _ => Nil): Seq[CheckError] =
    t.asSet.map(whenOk).getOrElse(Seq(TypeError(this, TSet(TAny()))))
  def checkBagThen(whenOk: TBag => Seq[CheckError] = _ => Nil): Seq[CheckError] =
    t.asBag.map(whenOk).getOrElse(Seq(TypeError(this, TBag(TAny()))))
  def checkPointerThen(whenOk: TPointer => Seq[CheckError] = _ => Nil): Seq[CheckError] =
    t.asPointer.map(whenOk).getOrElse(Seq(TypeError(this, TPointer(TAny()))))
  def checkArrayThen(whenOk: TArray => Seq[CheckError] = _ => Nil): Seq[CheckError] =
    t.asArray.map(whenOk).getOrElse(Seq(TypeError(this, TArray(TAny()))))
  def checkOptionThen(whenOk: TOption => Seq[CheckError] = _ => Nil): Seq[CheckError] =
    t.asOption.map(whenOk).getOrElse(Seq(TypeError(this, TOption(TAny()))))
  def checkMapThen(whenOk: TMap => Seq[CheckError] = _ => Nil): Seq[CheckError] =
    t.asMap.map(whenOk).getOrElse(Seq(TypeError(this, TMap(TAny(), TAny()))))
  def checkTupleThen(whenOk: TTuple => Seq[CheckError] = _ => Nil): Seq[CheckError] =
    t.asTuple.map(whenOk).getOrElse(Seq(TypeError(this, TTuple(Seq(TAny())))))
  /*def checkVectorThen(whenOk: TVector => Seq[CheckError] = _ => Nil): Seq[CheckError] =
    t.asVector.map(whenOk).getOrElse(Seq(TypeError(this, TVector(TAny()))))*/
  def checkMatrixThen(whenOk: TMatrix => Seq[CheckError] = _ => Nil): Seq[CheckError] =
    t.asMatrix.map(whenOk).getOrElse(Seq(TypeError(this, TMatrix(TAny()))))
  def checkModelThen(whenOk: TModel => Seq[CheckError] = _ => Nil): Seq[CheckError] =
    t.asModel.map(whenOk).getOrElse(Seq(TypeErrorText(this, got => s"Expected a model type, but got $got")))

  private def unfold(node: Expr)(matchFunc: PartialFunction[Expr, Seq[Expr]]): Seq[Expr] =
    matchFunc.lift(node) match {
      case Some(value) => value.flatMap(unfold(_)(matchFunc))
      case None => Seq(node)
    }
//
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
  implicit class IntegerValue(val value: BigInt)(implicit val o: Origin) extends Constant[BigInt] with Expr with NoCheck {
    override def t: Type = TBoundedInt(value, value + 1)
  }
  implicit class BooleanValue(val value: Boolean)(implicit val o: Origin) extends Constant[Boolean] with BoolExpr with NoCheck
}

case class LiteralSeq(element: Type, values: Seq[Expr])(implicit val o: Origin) extends Check(values.flatMap(_.checkSubType(element))) with Expr {
  override def t: Type = TSeq(element)
}

/* We do *not* want to statically deduplicate structurally equal expressions, since expressions may have side effects. */
case class LiteralSet(element: Type, values: Seq[Expr])(implicit val o: Origin) extends Check(values.flatMap(_.checkSubType(element))) with Expr {
  override def t: Type = TSet(element)
}

case class LiteralBag(element: Type, values: Seq[Expr])(implicit val o: Origin) extends Check(values.flatMap(_.checkSubType(element))) with Expr {
  override def t: Type = TBag(element)
}

case class LiteralTuple(ts: Seq[Type], values: Seq[Expr])(implicit val o: Origin) extends Expr {
  override def t: Type = TTuple(ts)
  override def check(context: CheckContext): Seq[CheckError] =
    if(ts.size == values.size) {
      values.zip(ts).flatMap { case (value, t) => value.checkSubType(t) }
    } else {
      Seq(TupleTypeCount(this))
    }
}

case class LiteralMap(k: Type, v: Type, values: Seq[(Expr, Expr)])(implicit val o: Origin) extends Expr {
  override def t: Type = TMap(k, v)
  override def check(context: CheckContext): Seq[CheckError] =
    values.flatMap { case (valueK, valueV) => valueK.checkSubType(k) ++ valueV.checkSubType(v) }
}

case class UntypedLiteralSeq(values: Seq[Expr])(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TSeq(Type.leastCommonSuperType(values.map(_.t)))
}

case class UntypedLiteralSet(values: Seq[Expr])(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TSet(Type.leastCommonSuperType(values.map(_.t)))
}

case class UntypedLiteralBag(values: Seq[Expr])(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TBag(Type.leastCommonSuperType(values.map(_.t)))
}

case class Void()(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TVoid()
}

case class ContextSensitiveNodeNotResolved(expr: Expr, message: String) extends ASTStateError {
  override def text: String =
    "A node was encountered of which the type is context-sensitive, but its context is not yet resolved. " +
    f"The node says: $message"
}

case class AmbiguousThis()(implicit val o: Origin) extends Expr with NoCheck {
  var ref: Option[Type] = None
  override def t: Type = ref.getOrElse(throw ContextSensitiveNodeNotResolved(this,
    "'this' encountered, but the surrounding class is not resolved."))
}

case class AmbiguousResult()(implicit val o: Origin) extends Expr with NoCheck {
  var ref: Option[Type] = None
  override def t: Type = ref.getOrElse(
    throw ContextSensitiveNodeNotResolved(this, "'\\result' encountered, but its attached method is not resolved."))
}

case class CurrentThreadId()(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TInt()
}

case class Null()(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TNull()
}

// The * in Perm(a[*], write)
case class Any()(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TInt()
}

case class NoPerm()(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TBoundedInt(0, 1)
}
case class ReadPerm()(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TFraction()
}
case class WritePerm()(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TBoundedInt(1, 2)
}

case class Range(from: Expr, to: Expr)(implicit val o: Origin) extends Check(from.checkSubType(TInt()), to.checkSubType(TInt())) with Expr {
  override def t: Type = TSeq(TInt())
}
case class Values(arr: Expr, from: Expr, to: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = TSeq(arr.t.asArray.get.element)
  override def check(context: CheckContext): Seq[CheckError] =
    from.checkSubType(TInt()) ++ to.checkSubType(TInt()) ++ arr.checkArrayThen()
}

case class OptSome(e: Expr)(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TOption(e.t)
}
case class OptNone()(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TOption(TAny())
}

case class MapCons(map: Expr, k: Expr, v: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = map.t
  override def check(context: CheckContext): Seq[CheckError] =
    map.checkMapThen(t => k.checkSubType(t.key) ++ v.checkSubType(t.key))
}
case class MapEq(left: Expr, right: Expr)(implicit val o: Origin) extends BoolExpr {
  override def check(context: CheckContext): Seq[CheckError] =
    left.checkMapThen(_ => right.checkMapThen(_ => Type.checkComparable(left, right)))
}
case class MapDisjoint(left: Expr, right: Expr)(implicit val o: Origin) extends BoolExpr {
  override def check(context: CheckContext): Seq[CheckError] =
    left.checkMapThen(_ => right.checkMapThen(_ => Type.checkComparable(left, right)))
}
case class MapKeySet(map: Expr)(implicit val o: Origin) extends Check(map.checkMapThen()) with Expr {
  override def t: Type = TSet(map.t.asMap.get.key)
}
case class MapValueSet(map: Expr)(implicit val o: Origin) extends Check(map.checkMapThen()) with Expr {
  override def t: Type = TSet(map.t.asMap.get.value)
}
case class MapItemSet(map: Expr)(implicit val o: Origin) extends Check(map.checkMapThen()) with Expr {
  override def t: Type = TSet(TTuple(Seq(map.t.asMap.get.key, map.t.asMap.get.value)))
}
case class MapSize(map: Expr)(implicit val o: Origin) extends Check(map.checkMapThen()) with IntExpr
case class MapRemove(map: Expr, k: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = map.t
  override def check(context: CheckContext): Seq[CheckError] =
    map.checkMapThen(t => k.checkSubType(t.key))
}

sealed trait Binder extends Declarator {
  def bindings: Seq[Variable]
  override def declarations: Seq[Declaration] = bindings
}

case class Forall(bindings: Seq[Variable], triggers: Seq[Seq[Expr]], body: Expr)(implicit val o: Origin) extends Check(body.checkSubType(TBool())) with BoolExpr with Binder
case class Starall(bindings: Seq[Variable], triggers: Seq[Seq[Expr]], body: Expr)(implicit val o: Origin) extends Check(body.checkSubType(TResource())) with ResourceExpr with Binder
case class Exists(bindings: Seq[Variable], triggers: Seq[Seq[Expr]], body: Expr)(implicit val o: Origin) extends Check(body.checkSubType(TBool())) with BoolExpr with Binder
case class Sum(bindings: Seq[Variable], condition: Expr, main: Expr)(implicit val o: Origin) extends Check(main.checkSubType(TInt()), condition.checkSubType(TBool())) with IntExpr with Binder
case class Product(bindings: Seq[Variable], condition: Expr, main: Expr)(implicit val o: Origin) extends Check(main.checkSubType(TInt()), condition.checkSubType(TBool())) with IntExpr with Binder
case class Let(binding: Variable, value: Expr, main: Expr)(implicit val o: Origin) extends Check(value.checkSubType(binding.t)) with Expr with Binder {
  override def t: Type = main.t
  override def bindings: Seq[Variable] = Seq(binding)
}
case class InlinePattern(inner: Expr)(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = inner.t
}

case class Local(ref: Ref[Variable])(implicit val o: Origin) extends Expr {
  override def t: Type = ref.decl.t
  override def check(context: CheckContext): Seq[CheckError] =
    context.checkInScope(this, ref)
}
trait HeapDeref
case class Deref(obj: Expr, ref: Ref[Field])(val blame: Blame[InsufficientPermission])(implicit val o: Origin) extends Expr with HeapDeref {
  override def t: Type = ref.decl.t
  override def check(context: CheckContext): Seq[CheckError] =
    context.checkInScope(this, ref)
}
case class ModelDeref(obj: Expr, ref: Ref[ModelField])(val blame: Blame[ModelInsufficientPermission])(implicit val o: Origin) extends Expr {
  override def t: Type = ref.decl.t
  override def check(context: CheckContext): Seq[CheckError] =
    context.checkInScope(this, ref)
}
case class DerefPointer(pointer: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = pointer.t.asPointer.get.element
  override def check(context: CheckContext): Seq[CheckError] =
    pointer.checkPointerThen()
}
case class AddrOf(e: Expr)(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TPointer(e.t)
}

sealed trait Apply extends Expr {
  def ref: Ref[Applicable]
  def args: Seq[Expr]

  override def t: Type = ref.decl.returnType

  override def check(context: CheckContext): Seq[CheckError] =
    ref.decl.args.zip(args).flatMap {
      case (arg, value) => value.checkSubType(arg.t)
    }
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

  override def check(context: CheckContext): Seq[CheckError] =
    typeArgs match {
      case None => super.check(context)
      case Some((adt, typeArgs)) =>
        ref.decl.args.zip(args).flatMap {
          case (arg, value) => value.checkSubType(arg.t.particularize(adt.decl.typeArgs.zip(typeArgs).toMap))
        }
    }
}

sealed trait Invocation extends Apply {
  override def ref: Ref[ContractApplicable]
  def blame: Blame[PreconditionFailed]
  def typeArgs: Seq[Type]

  override def t: Type = super.t.particularize(ref.decl.typeArgs.zip(typeArgs).toMap)

  override def check(context: CheckContext): Seq[CheckError] =
    ref.decl.args.zip(args).flatMap {
      case (arg, value) => value.checkSubType(arg.t.particularize(ref.decl.typeArgs.zip(typeArgs).toMap))
    }
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
  def t: Type = arg.t
}

/* The "unary plus" is the identity function, e.g. +1 is UPlus(IntegerValue(1)) */
case class UPlus(arg: Expr)(implicit val o: Origin) extends Check(arg.checkSubType(TRational())) with UnExpr
case class UMinus(arg: Expr)(implicit val o: Origin) extends Check(arg.checkSubType(TRational())) with UnExpr
case class BitNot(arg: Expr)(implicit val o: Origin) extends Check(arg.checkSubType(TInt())) with UnExpr
case class Not(arg: Expr)(implicit val o: Origin) extends Check(arg.checkSubType(TBool())) with UnExpr

sealed trait BinExpr extends Expr {
  def left: Expr
  def right: Expr
}
sealed trait NumericBinExpr extends BinExpr {
  override def check(context: CheckContext): Seq[CheckError] =
    left.checkSubType(TRational()) ++ right.checkSubType(TRational())

  override def t: Type = Type.leastCommonSuperType(left.t, right.t)
}

sealed trait IntBinExpr extends BinExpr {
  override def t: Type = TInt()
  override def check(context: CheckContext): Seq[CheckError] =
    left.checkSubType(TInt()) ++ right.checkSubType(TInt())
}

sealed trait BoolBinExpr extends BinExpr {
  override def t: Type = TBool()
  override def check(context: CheckContext): Seq[CheckError] =
    left.checkSubType(TBool()) ++ right.checkSubType(TBool())
}

sealed trait DividingExpr extends Expr {
  def blame: Blame[DivByZero]
}

case class AmbiguousMult(left: Expr, right: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = Type.leastCommonSuperType(left.t, right.t)

  override def check(context: CheckContext): Seq[CheckError] = left.t match {
    case TProcess() => right.checkSubType(TProcess())
    case _ => left.checkSubType(TRational()) ++ right.checkSubType(TRational())
  }
}

case class AmbiguousPlus(left: Expr, right: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = Type.leastCommonSuperType(left.t, right.t)

  override def check(context: CheckContext): Seq[CheckError] = left.t match {
    case TProcess() => right.checkSubType(TProcess())
    case TSeq(_) => right.checkSeqThen()
    case TBag(_) => right.checkBagThen()
    case TSet(_) => right.checkSetThen()
    case TPointer(_) => right.checkSubType(TInt())
    case _ => left.checkSubType(TRational()) ++ right.checkSubType(TRational())
  }
}


case class AmbiguousOr(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr {
  override def t: Type = Type.leastCommonSuperType(left.t, right.t)

  override def check(context: CheckContext): Seq[CheckError] = left.t match {
    case TProcess() => right.checkSubType(TProcess())
    case _ => left.checkSubType(TBool()) ++ right.checkSubType(TBool())
  }
}

sealed trait BitOp extends BinExpr {
  override def t: Type = left.t

  override def check(context: CheckContext): Seq[CheckError] = left.t.mimics match {
    case TBool() => right.checkSubType(TBool())
    case _ => left.checkSubType(TInt()) ++ right.checkSubType(TInt())
  }
}

case class AmbiguousComputationalOr(left: Expr, right: Expr)(implicit val o: Origin) extends BitOp
case class AmbiguousComputationalXor(left: Expr, right: Expr)(implicit val o: Origin) extends BitOp
case class AmbiguousComputationalAnd(left: Expr, right: Expr)(implicit val o: Origin) extends BitOp

case class Exp(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr
case class Plus(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr
case class Minus(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr
case class Mult(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr
case class Div(left: Expr, right: Expr)(val blame: Blame[DivByZero])(implicit val o: Origin) extends NumericBinExpr with DividingExpr
case class FloorDiv(left: Expr, right: Expr)(val blame: Blame[DivByZero])(implicit val o: Origin) extends IntBinExpr with DividingExpr
case class Mod(left: Expr, right: Expr)(val blame: Blame[DivByZero])(implicit val o: Origin) extends NumericBinExpr with DividingExpr
case class BitAnd(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr
case class BitOr(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr
case class BitXor(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr
case class BitShl(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr
case class BitShr(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr // sign-extended (signed)
case class BitUShr(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr // not sign-extended (unsigned)

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
case class Implies(left: Expr, right: Expr)(implicit val o: Origin) extends Check(left.checkSubType(TBool()), right.checkSubType(TResource())) with BinExpr {
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
case class Star(left: Expr, right: Expr)(implicit val o: Origin) extends Check(left.checkSubType(TResource()), right.checkSubType(TResource())) with ResourceExpr
case class Wand(left: Expr, right: Expr)(implicit val o: Origin) extends Check(left.checkSubType(TResource()), right.checkSubType(TResource())) with ResourceExpr
case class Scale(scale: Expr, res: Expr)(implicit val o: Origin) extends Check(scale.checkSubType(TRational()), res.checkSubType(TResource())) with ResourceExpr
case class Unfolding(pred: Expr, body: Expr)(implicit val o: Origin) extends Check(pred.checkSubType(TResource())) with Expr {
  override def t: Type = body.t
}

case class Perm(loc: Expr, perm: Expr)(implicit val o: Origin) extends Check(perm.checkSubType(TRational())) with ResourceExpr
case class HPerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends Check(perm.checkSubType(TRational())) with ResourceExpr
case class APerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends Check(perm.checkSubType(TRational())) with ResourceExpr

case class PointsTo(loc: Expr, perm: Expr, value: Expr)(implicit val o: Origin) extends Check(perm.checkSubType(TRational()), value.checkSubType(loc.t)) with ResourceExpr
case class CurPerm(loc: Expr)(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TRational()
}

case class ValidArray(arr: Expr, len: Expr)(implicit val o: Origin) extends BoolExpr {
  override def check(context: CheckContext): Seq[CheckError] =
    len.checkSubType(TInt()) ++ (arr.t match {
      case TArray(_) => Seq()
      case _ => Seq(TypeError(arr, TArray(TAny())))
    })
}
case class ValidMatrix(mat: Expr, w: Expr, h: Expr)(implicit val o: Origin) extends BoolExpr {
  override def check(context: CheckContext): Seq[CheckError] =
    w.checkSubType(TInt()) ++ h.checkSubType(TInt()) ++ (mat.t match {
      case TArray(TArray(_)) => Seq()
      case _ => Seq(TypeError(mat, TArray(TArray(TAny()))))
    })
}

case class PermPointer(p: Expr, len: Expr, perm: Expr)(implicit val o: Origin) extends ResourceExpr {
  override def check(context: CheckContext): Seq[CheckError] =
    len.checkSubType(TInt()) ++ perm.checkSubType(TRational()) ++ (p.t match {
      case TPointer(_) => Seq()
      case _ => Seq(TypeError(p, TPointer(TAny())))
    })
}
case class PermPointerIndex(p: Expr, idx: Expr, perm: Expr)(implicit val o: Origin) extends ResourceExpr {
  override def check(context: CheckContext): Seq[CheckError] =
    idx.checkSubType(TInt()) ++ perm.checkSubType(TRational()) ++ (p.t match {
      case TPointer(_) => Seq()
      case _ => Seq(TypeError(p, TPointer(TAny())))
    })
}

sealed trait Comparison extends BinExpr {
  override def t: Type = TBool()
  override def check(context: CheckContext): Seq[CheckError] =
    Type.checkComparable(left, right)
}

case class Eq(left: Expr, right: Expr)(implicit val o: Origin) extends Comparison
case class Neq(left: Expr, right: Expr)(implicit val o: Origin) extends Comparison

sealed trait OrderOp extends Comparison {
  override def check(context: CheckContext): Seq[CheckError] = {
    left.t match {
      case TSet(leftT) => right.checkSetThen(rightSet =>
        if(Type.isComparable(leftT, rightSet.element)) Nil else Seq(IncomparableTypes(left, right)))
      case TBag(leftT) => right.checkBagThen(rightBag =>
        if(Type.isComparable(leftT, rightBag.element)) Nil else Seq(IncomparableTypes(left, right)))
      case _ => left.checkSubType(TRational()) ++ right.checkSubType(TRational())
    }
  }
}

case class Greater(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp
case class Less(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp
case class GreaterEq(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp
case class LessEq(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp

case class Select(condition: Expr, whenTrue: Expr, whenFalse: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type =
    Type.leastCommonSuperType(whenTrue.t, whenFalse.t)
  override def check(context: CheckContext): Seq[CheckError] =
    condition.checkSubType(TBool()) ++ Type.checkComparable(whenTrue, whenFalse)
}

case class NewObject(cls: Ref[Class])(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TClass(cls)
}

case class NewArray(element: Type, dims: Seq[Expr], moreDims: Int)(implicit val o: Origin) extends Check(dims.flatMap(_.checkSubType(TInt()))) with Expr {
  override def t: Type = FuncTools.repeat(TArray(_), dims.size + moreDims, element)
}

case class Old(expr: Expr, at: Option[Ref[LabelDecl]])(val blame: Blame[LabelNotReached])(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = expr.t
}

case class AmbiguousSubscript(collection: Expr, index: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = collection.t match {
    case TArray(t) => t
    case TSeq(t) => t
    case TPointer(t) => t
    case _ => throw UnreachableAfterTypeCheck("AmbiguousSubscript should subscript an array, sequence or pointer.", this)
  }

  override def check(context: CheckContext): Seq[CheckError] =
    collection.t match {
      case TArray(_) | TSeq(_) | TPointer(_) =>
        index.checkSubType(TInt())
      case _ =>
        Seq(TypeErrorText(collection,
          got =>
            "Expected a subscriptable expression of type " +
            s"${TArray(TAny())}, ${TSeq(TAny())} or ${TPointer(TAny())}, " +
            s"but got $got"))
    }
}

case class SeqSubscript(seq: Expr, index: Expr)(val blame: Blame[SeqBoundFailure])(implicit val o: Origin) extends Check(seq.checkSeqThen(), index.checkSubType(TInt())) with Expr {
  override def t: Type = seq.t.asSeq.get.element
}

case class ArraySubscript(arr: Expr, index: Expr)(implicit val o: Origin) extends Check(arr.checkArrayThen(), index.checkSubType(TInt())) with Expr {
  override def t: Type = arr.t.asArray.get.element
}

case class PointerSubscript(pointer: Expr, index: Expr)(implicit val o: Origin) extends Check(pointer.checkPointerThen(), index.checkSubType(TInt())) with Expr {
  override def t: Type = pointer.t.asPointer.get.element
}

case class Length(arr: Expr)(implicit val o: Origin) extends IntExpr {
  override def check(context: CheckContext): Seq[CheckError] =
    arr.t match {
      case TArray(_) => Seq()
      case _ => Seq(TypeError(arr, TArray(TAny())))
    }
}

case class Size(obj: Expr)(implicit val o: Origin) extends IntExpr {
  override def check(context: CheckContext): Seq[CheckError] =
    obj.t match {
      case _: CollectionType => Seq()
      case _ => Seq(TypeErrorText(obj, got => s"Expected a collection type such as a sequence or map, but got $got"))
    }
}

case class Cons(x: Expr, xs: Expr)(implicit val o: Origin) extends Check(xs.checkSeqThen(t => x.checkSubType(t.element))) with Expr {
  override def t: Type = xs.t
}
case class Head(xs: Expr)(implicit val o: Origin) extends Check(xs.checkSeqThen()) with Expr {
  override def t: Type = xs.t.asSeq.get.element
}
case class Tail(xs: Expr)(implicit val o: Origin) extends Check(xs.checkSeqThen()) with Expr {
  override def t: Type = xs.t
}
case class Drop(xs: Expr, count: Expr)(implicit val o: Origin) extends Check(xs.checkSeqThen(), count.checkSubType(TInt())) with Expr {
  override def t: Type = xs.t
}
case class Take(xs: Expr, count: Expr)(implicit val o: Origin) extends Check(xs.checkSeqThen(), count.checkSubType(TInt())) with Expr {
  override def t: Type = xs.t
}
case class Slice(xs: Expr, from: Expr, to: Expr)(implicit val o: Origin) extends Check(xs.checkSeqThen(), from.checkSubType(TInt()), to.checkSubType(TInt())) with Expr {
  override def t: Type = xs.t
}
case class SeqUpdate(xs: Expr, i: Expr, x: Expr)(implicit val o: Origin) extends Check(xs.checkSeqThen(t => x.checkSubType(t.element)), i.checkSubType(TInt())) with Expr {
  override def t: Type = xs.t
}
case class Concat(xs: Expr, ys: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = Type.leastCommonSuperType(xs.t.asSeq.get, ys.t.asSeq.get)
  override def check(context: CheckContext): Seq[CheckError] =
    xs.checkSeqThen() ++ ys.checkSeqThen() ++ Type.checkComparable(xs, ys)
}
case class RemoveAt(xs: Expr, i: Expr)(implicit val o: Origin) extends Check(xs.checkSeqThen(), i.checkSubType(TInt())) with Expr {
  override def t: Type = xs.t
}
case class Empty(obj: Expr)(implicit val o: Origin) extends IntExpr {
  override def check(context: CheckContext): Seq[CheckError] =
    obj.t match {
      case _: CollectionType => Seq()
      case _ => Seq(TypeErrorText(obj, got => s"Expected a collection type such as a sequence or map, but got $got"))
    }
}

case class AmbiguousMember(x: Expr, xs: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type =
    xs.t match {
      case TSeq(_) | TSet(_) | TMap(_, _) => TBool()
      case TBag(_) => TInt()
      case _ => throw UnreachableAfterTypeCheck("AmbiguousMember expects a collection type", this)
    }

  override def check(context: CheckContext): Seq[CheckError] =
    xs.t match {
      case TSeq(xt) => x.checkSubType(xt)
      case TSet(xt) => x.checkSubType(xt)
      case TBag(xt) => x.checkSubType(xt)
      case TMap(xt, _) => x.checkSubType(xt)
      case _ => Seq(TypeErrorText(x, got => s"Expected a collection type such as a sequence or map, but got $got"))
    }
}
case class SetMember(x: Expr, xs: Expr)(implicit val o: Origin) extends Check(xs.checkSetThen(set => x.checkSubType(set.element))) with BoolExpr
case class SeqMember(x: Expr, xs: Expr)(implicit val o: Origin) extends Check(xs.checkSeqThen(seq => x.checkSubType(seq.element))) with BoolExpr
case class MapMember(x: Expr, xs: Expr)(implicit val o: Origin) extends Check(xs.checkMapThen(map => x.checkSubType(map.key))) with BoolExpr
case class BagMemberCount(x: Expr, xs: Expr)(implicit val o: Origin) extends Check(xs.checkMapThen()) with IntExpr

sealed trait SetComparison extends Comparison {
  override def check(context: CheckContext): Seq[CheckError] =
    left.checkSetThen() ++ right.checkSetThen() ++ Type.checkComparable(left, right)
}
case class SubSet(left: Expr, right: Expr)(implicit val o: Origin) extends SetComparison
case class SubSetEq(left: Expr, right: Expr)(implicit val o: Origin) extends SetComparison
case class Permutation(xs: Expr, ys: Expr)(implicit val o: Origin) extends BoolExpr {
  override def check(context: CheckContext): Seq[CheckError] =
    xs.checkSeqThen() ++ ys.checkSeqThen() ++ Type.checkComparable(xs, ys)
}
case class OptGet(opt: Expr)(implicit val o: Origin) extends Check(opt.checkOptionThen()) with Expr {
  override def t: Type = opt.t.asOption.get.element
}
case class OptGetOrElse(opt: Expr, alt: Expr)(implicit val o: Origin) extends Check(opt.checkOptionThen(t => alt.checkSubType(t.element))) with Expr {
  override def t: Type = Type.leastCommonSuperType(opt.t.asOption.get.element, alt.t)
}
case class MapGet(map: Expr, k: Expr)(implicit val o: Origin) extends Check(map.checkMapThen(t => k.checkSubType(t.key))) with Expr {
  override def t: Type = map.t.asMap.get.value
}
case class TupGet(tup: Expr, index: Int)(implicit val o: Origin) extends Expr {
  override def t: Type = tup.t.asTuple.get.elements(index)
  override def check(context: CheckContext): Seq[CheckError] =
    tup.checkTupleThen(t => if(0 <= index && index < t.elements.size) Seq() else Seq(TypeErrorText(this, _ => "Tuple getter exceeds tuple size")))
}

case class VectorSum(indices: Expr, vec: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = vec.t.asSeq.get.element
  override def check(context: CheckContext): Seq[CheckError] = vec.checkSubType(TSeq(TRational()))
}
case class VectorCompare(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr {
  override def t: Type = TSeq(TInt()) // the results are 0 or 1, mimicking TSeq(TBool())
  override def check(context: CheckContext): Seq[CheckError] =
    left.checkSeqThen(leftT => right.checkSeqThen(rightT =>
      if(Type.isComparable(leftT.element, rightT.element)) Nil
      else Seq(IncomparableTypes(left, right))
    ))
}
case class VectorRepeat(e: Expr)(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TSeq(e.t)
}
case class MatrixSum(indices: Expr, mat: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = mat.t.asMatrix.get.element
  override def check(context: CheckContext): Seq[CheckError] = mat.checkSubType(TMatrix(TRational()))
}
case class MatrixCompare(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr {
  override def t: Type = TMatrix(TInt())
  override def check(context: CheckContext): Seq[CheckError] =
    left.checkMatrixThen(leftT => right.checkMatrixThen(rightT =>
      if(Type.isComparable(leftT.element, rightT.element)) Nil
      else Seq(IncomparableTypes(left, right))
    ))
}
case class MatrixRepeat(e: Expr)(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TMatrix(e.t)
}

case class TypeValue(value: Type)(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TType(value)
}
case class TypeOf(expr: Expr)(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TType(expr.t)
}
case class InstanceOf(value: Expr, typeValue: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = TBool()
  override def check(context: CheckContext): Seq[CheckError] =
    typeValue.t match {
      case TType(t) =>
        if(Type.isComparable(value.t, t))
          Seq()
        else
          Seq(IncomparableTypes(value, typeValue))
      case _ =>
        Seq(TypeError(typeValue, TType(value.t)))
    }
}
case class Cast(value: Expr, typeValue: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type = typeValue.t match {
    case TType(t) =>
      t
    case _ => throw UnreachableAfterTypeCheck("The cast type is not a type", this)
  }
  override def check(context: CheckContext): Seq[CheckError] =
    typeValue.t match {
      case TType(t) =>
        if(Type.isComparable(value.t, t))
          Seq()
        else
          Seq(IncomparableTypes(value, typeValue))
      case _ =>
        Seq(TypeError(typeValue, TType(value.t)))
    }
}

sealed trait TypeComparison extends Expr {
  def left: Expr
  def right: Expr

  override def t: Type = TBool()
  override def check(context: CheckContext): Seq[CheckError] =
    Type.checkComparable(left, right)
}

case class SubType(left: Expr, right: Expr)(implicit val o: Origin) extends TypeComparison
case class SuperType(left: Expr, right: Expr)(implicit val o: Origin) extends TypeComparison

sealed trait AssignExpression extends Expr {
  def target: Expr
  def value: Expr

  override def check(context: CheckContext): Seq[CheckError] =
    value.checkSubType(target.t)
}

case class PreAssignExpression(target: Expr, value: Expr)(implicit val o: Origin) extends AssignExpression {
  override def t: Type = value.t
}
case class PostAssignExpression(target: Expr, value: Expr)(implicit val o: Origin) extends AssignExpression {
  override def t: Type = target.t
}

case class With(pre: Statement, value: Expr)(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = value.t
}
case class Then(value: Expr, post: Statement)(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = value.t
}

case class Held(obj: Expr)(implicit val o: Origin) extends Check(obj.checkClassType) with BoolExpr
case class IdleToken(thread: Expr)(implicit val o: Origin) extends Check(thread.checkClassType) with BoolExpr
case class JoinToken(thread: Expr)(implicit val o: Origin) extends Check(thread.checkClassType) with BoolExpr

case class EmptyProcess()(implicit val o: Origin) extends ProcessExpr with NoCheck
case class ActionApply(action: Ref[ModelAction], args: Seq[Expr])(implicit val o: Origin) extends ProcessExpr with NoCheck
case class ProcessApply(process: Ref[ModelProcess], args: Seq[Expr])(implicit val o: Origin) extends ProcessExpr with NoCheck
case class ProcessSeq(left: Expr, right: Expr)(implicit val o: Origin)
  extends Check(left.checkSubType(TProcess()), right.checkSubType(TProcess())) with ProcessExpr
case class ProcessChoice(left: Expr, right: Expr)(implicit val o: Origin)
  extends Check(left.checkSubType(TProcess()), right.checkSubType(TProcess())) with ProcessExpr
case class ProcessPar(left: Expr, right: Expr)(implicit val o: Origin)
  extends Check(left.checkSubType(TProcess()), right.checkSubType(TProcess())) with ProcessExpr
case class ProcessSelect(cond: Expr, whenTrue: Expr, whenFalse: Expr)(implicit val o: Origin)
  extends Check(cond.checkSubType(TBool()), whenTrue.checkSubType(TProcess()), whenFalse.checkSubType(TProcess())) with ProcessExpr

case class ModelNew(ref: Ref[Model])(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TModel(ref)
}

case class ModelState(model: Expr, perm: Expr, state: Expr)(implicit val o: Origin) extends ResourceExpr {
  override def check(context: CheckContext): Seq[CheckError] = model.checkModelThen() ++ state.checkSubType(TProcess()) ++ perm.checkSubType(TRational())
}
case class ModelAbstractState(model: Expr, state: Expr)(implicit val o: Origin) extends ResourceExpr {
  override def check(context: CheckContext): Seq[CheckError] = model.checkModelThen() ++ state.checkSubType(TResource())
}
case class ModelCreate(model: Expr, init: Expr)(implicit val o: Origin) extends VoidExpr {
  override def check(context: CheckContext): Seq[CheckError] = model.checkModelThen() ++ init.checkSubType(TProcess())
}
case class ModelDestroy(model: Expr)(implicit val o: Origin) extends VoidExpr {
  override def check(context: CheckContext): Seq[CheckError] = model.checkModelThen()
}
case class ModelSplit(model: Expr, leftPerm: Expr, leftProcess: Expr, rightPerm: Expr, rightProcess: Expr)(implicit val o: Origin) extends VoidExpr {
  override def check(context: CheckContext): Seq[CheckError] =
    model.checkModelThen() ++
      leftProcess.checkSubType(TProcess()) ++ rightProcess.checkSubType(TProcess()) ++
      leftPerm.checkSubType(TRational()) ++ rightPerm.checkSubType(TRational())
}
case class ModelMerge(model: Expr, leftPerm: Expr, leftProcess: Expr, rightPerm: Expr, rightProcess: Expr)(implicit val o: Origin) extends VoidExpr {
  override def check(context: CheckContext): Seq[CheckError] =
    model.checkModelThen() ++
      leftProcess.checkSubType(TProcess()) ++ rightProcess.checkSubType(TProcess()) ++
      leftPerm.checkSubType(TRational()) ++ rightPerm.checkSubType(TRational())
}
case class ModelChoose(model: Expr, perm: Expr, totalProcess: Expr, choice: Expr)(implicit val o: Origin) extends VoidExpr {
  override def check(context: CheckContext): Seq[CheckError] =
    model.checkModelThen() ++ perm.checkSubType(TRational()) ++
      totalProcess.checkSubType(TProcess()) ++ choice.checkSubType(TProcess())
}

case class ModelPerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends Check(perm.checkSubType(TRational())) with ResourceExpr
case class ActionPerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends Check(perm.checkSubType(TRational())) with ResourceExpr

