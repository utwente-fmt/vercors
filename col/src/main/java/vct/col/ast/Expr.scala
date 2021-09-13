package vct.col.ast

sealed trait Expr extends NodeFamily {
  def t: Type

  def checkSubType(other: Type): Seq[CheckError] =
    if(other.superTypeOf(t))
      Nil
    else
      Seq(TypeError(this, other))

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
  def checkModelThen(whenOk: TModel => Seq[CheckError] = _ => Nil): Seq[CheckError] =
    t.asModel.map(whenOk).getOrElse(Seq(TypeErrorText(this, got => s"Expected a model type, but got $got")))
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

sealed abstract class Constant[T] extends Expr {
  def value: T

  override def equals(obj: Any): Boolean = obj match {
    case const: Constant[T] => this.value == const.value
  }

  override def hashCode(): Int = value.hashCode()
  override def toString: String = value.toString
}

object Constant {
  def unapply(obj: Any): Option[Any] = obj match {
    case c: Constant[_] => Some(c.value)
    case _ => None
  }

  implicit class IntegerValue(val value: Int)(implicit val o: Origin) extends Constant[Int] with IntExpr with NoCheck
  implicit class BooleanValue(val value: Boolean)(implicit val o: Origin) extends Constant[Boolean] with BoolExpr with NoCheck
}

case class LiteralSeq(element: Type, values: Seq[Expr])(implicit val o: Origin) extends Check(values.flatMap(_.checkSubType(element))) with Expr {
  override def t: Type = TSeq(element)
}

/* We do *not* want to statically deduplicate structurally equal expressions, since expressions may have side effects. */
case class LiteralSet(element: Type, values: Seq[Expr])(implicit val o: Origin) extends Check(values.flatMap(_.checkSubType(element))) with Expr {
  override def t: Type = TSet(element)
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

case class NoPerm()(implicit val o: Origin) extends RationalExpr with NoCheck
case class ReadPerm()(implicit val o: Origin) extends RationalExpr with NoCheck
case class WritePerm()(implicit val o: Origin) extends RationalExpr with NoCheck

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
    context.inScope(ref)
}
case class Deref(obj: Expr, ref: Ref[Field])(implicit val o: Origin) extends Expr {
  override def t: Type = ref.decl.t
  override def check(context: CheckContext): Seq[CheckError] =
    context.inScope(ref)
}
case class ModelDeref(obj: Expr, ref: Ref[ModelField])(implicit val o: Origin) extends Expr {
  override def t: Type = ref.decl.t
  override def check(context: CheckContext): Seq[CheckError] =
    context.inScope(ref)
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

case class ADTFunctionInvocation(ref: Ref[ADTFunction], args: Seq[Expr])(implicit val o: Origin) extends Apply

sealed trait Invocation extends Apply {
  def blame: PreconditionBlame
}

case class ProcedureInvocation(ref: Ref[Procedure], args: Seq[Expr], outArgs: Seq[Ref[Variable]])
                              (val blame: PreconditionBlame)(implicit val o: Origin) extends Invocation
case class FunctionInvocation(ref: Ref[Function], args: Seq[Expr])
                             (val blame: PreconditionBlame)(implicit val o: Origin) extends Invocation

case class MethodInvocation(obj: Expr, ref: Ref[InstanceMethod], args: Seq[Expr], outArgs: Seq[Ref[Variable]])
                           (val blame: PreconditionBlame)(implicit val o: Origin) extends Invocation
case class InstanceFunctionInvocation(obj: Expr, ref: Ref[InstanceFunction], args: Seq[Expr])
                                     (val blame: PreconditionBlame)(implicit val o: Origin) extends Invocation

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
  def blame: DivByZeroBlame
}

case class Exp(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr
case class Plus(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr
case class Minus(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr
case class Mult(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr
case class Div(left: Expr, right: Expr)(val blame: DivByZeroBlame)(implicit val o: Origin) extends NumericBinExpr with DividingExpr
case class FloorDiv(left: Expr, right: Expr)(val blame: DivByZeroBlame)(implicit val o: Origin) extends IntBinExpr with DividingExpr
case class Mod(left: Expr, right: Expr)(val blame: DivByZeroBlame)(implicit val o: Origin) extends NumericBinExpr with DividingExpr
case class BitAnd(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr
case class BitOr(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr
case class BitXor(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr
case class BitShl(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr
case class BitShr(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr // sign-extended (signed)
case class BitUShr(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr // not sign-extended (unsigned)

case class And(left: Expr, right: Expr)(implicit val o: Origin) extends BoolBinExpr
case class Or(left: Expr, right: Expr)(implicit val o: Origin) extends BoolBinExpr
case class Implies(left: Expr, right: Expr)(implicit val o: Origin) extends Check(left.checkSubType(TBool()), right.checkSubType(TResource())) with BinExpr {
  override def t: Type = right.t
}
object Star {
  def fold(exprs: Seq[Expr])(implicit o: Origin): Expr = exprs match {
    case Nil => Constant.BooleanValue(true)
    case more => more.reduceLeft(new Star(_, _))
  }
}
case class Star(left: Expr, right: Expr)(implicit val o: Origin) extends Check(left.checkSubType(TResource()), right.checkSubType(TResource())) with ResourceExpr
case class Wand(left: Expr, right: Expr)(implicit val o: Origin) extends Check(left.checkSubType(TResource()), right.checkSubType(TResource())) with ResourceExpr
case class Scale(scale: Expr, res: Expr)(implicit val o: Origin) extends Check(scale.checkSubType(TRational()), res.checkSubType(TResource())) with ResourceExpr
case class Unfolding(pred: Expr, body: Expr)(implicit val o: Origin) extends Check(pred.checkSubType(TResource())) with Expr {
  override def t: Type = body.t
}

case class Perm(loc: Expr, perm: Expr)(implicit val o: Origin) extends Check(perm.checkSubType(TRational())) with ResourceExpr
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

sealed trait NumericComparison extends Comparison {
  override def check(context: CheckContext): Seq[CheckError] =
    left.checkSubType(TRational()) ++ right.checkSubType(TRational())
}

case class Greater(left: Expr, right: Expr)(implicit val o: Origin) extends NumericComparison
case class Less(left: Expr, right: Expr)(implicit val o: Origin) extends NumericComparison
case class GreaterEq(left: Expr, right: Expr)(implicit val o: Origin) extends NumericComparison
case class LessEq(left: Expr, right: Expr)(implicit val o: Origin) extends NumericComparison

case class Select(condition: Expr, whenTrue: Expr, whenFalse: Expr)(implicit val o: Origin) extends Expr {
  override def t: Type =
    Type.leastCommonSuperType(whenTrue.t, whenFalse.t)
  override def check(context: CheckContext): Seq[CheckError] =
    condition.checkSubType(TBool()) ++ Type.checkComparable(whenTrue, whenFalse)
}

case class NewObject(cls: Ref[Class])(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TClass(cls)
}

case class NewArray(element: Type, dims: Seq[Expr])(implicit val o: Origin) extends Check(dims.flatMap(_.checkSubType(TInt()))) with Expr {
  override def t: Type = TArray(element)
}

case class Old(expr: Expr, at: Option[Ref[LabelDecl]])(val blame: LabelNotReachedBlame)(implicit val o: Origin) extends Expr with NoCheck {
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

case class SeqSubscript(seq: Expr, index: Expr)(val blame: SeqBoundsBlame)(implicit val o: Origin) extends Check(seq.checkSeqThen(), index.checkSubType(TInt())) with Expr {
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

case class Held(obj: Expr)(implicit val o: Origin) extends Check(obj.checkSubType(TClass.OBJECT)) with BoolExpr
case class IdleToken(thread: Expr)(implicit val o: Origin) extends Check(thread.checkSubType(TClass.RUNNABLE)) with BoolExpr
case class JoinToken(thread: Expr)(implicit val o: Origin) extends Check(thread.checkSubType(TClass.RUNNABLE)) with BoolExpr

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

case class ModelPerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends Check(perm.checkSubType(TRational())) with ResourceExpr
case class ActionPerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends Check(perm.checkSubType(TRational())) with ResourceExpr

