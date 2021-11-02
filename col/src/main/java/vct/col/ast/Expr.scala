package vct.col.ast

import hre.util.FuncTools
import vct.col.resolve.{RefAxiomaticDataType, RefJavaClass, RefModel, SpecTypeNameTarget}

sealed trait Expr extends NodeFamily {
  def t: Type

  private def isOfClassType: Boolean = t.mimics match {
    case TClass(_) => true
    case t @ JavaTClass(_) => t.ref.get match {
      case RefAxiomaticDataType(_) => false
      case RefModel(_) => false
      case RefJavaClass(_) => true
    }
    case _ => false
  }

  private def unfold(node: Expr)(matchFunc: PartialFunction[Expr, Seq[Expr]]): Seq[Expr] =
    matchFunc.lift(node) match {
      case Some(value) => value.flatMap(unfold(_)(matchFunc))
      case None => Seq(node)
    }

  def unfoldStar: Seq[Expr] = unfold(this) { case Star(left, right) => Seq(left, right) }
  def unfoldProcessPar: Seq[Expr] = unfold(this) { case ProcessPar(l, r) => Seq(l, r) }
}

sealed trait CoercingExpr extends Expr {
  def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr

  def check(context: CheckContext): Seq[CheckError] =
    try {
      case object DummyScope extends ScopeContext
      coerce(NopCoercionResolver)(DiagnosticOrigin, DummyScope)
      Nil
    } catch {
      case Incoercible(expr, target) => Seq(TypeError(expr, target))
      case IncoercibleText(expr, message) => Seq(TypeErrorText(expr, message))
    }
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

case class LiteralSeq(element: Type, values: Seq[Expr])(implicit val o: Origin) extends CoercingExpr {
  override def t: Type = TSeq(element)
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    LiteralSeq(element, values.map(resolver(_, element)))
}

/* We do *not* want to statically deduplicate structurally equal expressions, since expressions may have side effects. */
case class LiteralSet(element: Type, values: Seq[Expr])(implicit val o: Origin) extends CoercingExpr {
  override def t: Type = TSet(element)

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    LiteralSet(element, values.map(resolver(_, element)))
}

case class LiteralBag(element: Type, values: Seq[Expr])(implicit val o: Origin) extends CoercingExpr {
  override def t: Type = TBag(element)
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    LiteralBag(element, values.map(resolver(_, element)))
}

case class LiteralTuple(ts: Seq[Type], values: Seq[Expr])(implicit val o: Origin) extends CoercingExpr {
  override def t: Type = TTuple(ts)

  override def check(context: CheckContext): Seq[CheckError] =
    if(ts.size == values.size) {
      super.check(context)
    } else {
      Seq(TupleTypeCount(this))
    }

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    LiteralTuple(ts, values.zip(ts).map {
      case (value, t) => resolver(value, t)
    })
}

case class LiteralMap(k: Type, v: Type, values: Seq[(Expr, Expr)])(implicit val o: Origin) extends CoercingExpr {
  override def t: Type = TMap(k, v)
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    LiteralMap(k, v, values.map {
      case (key, value) => (resolver(key, k), resolver(value, v))
    })
}

case class UntypedLiteralSeq(values: Seq[Expr])(implicit val o: Origin) extends CoercingExpr {
  def elementType: Type = Type.leastCommonSuperType(values.map(_.t))
  override def t: Type = TSeq(elementType)
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    UntypedLiteralSeq(values.map(resolver(_, elementType)))
}

case class UntypedLiteralSet(values: Seq[Expr])(implicit val o: Origin) extends CoercingExpr {
  def elementType: Type = Type.leastCommonSuperType(values.map(_.t))
  override def t: Type = TSet(elementType)

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    UntypedLiteralSet(values.map(resolver(_, elementType)))
}

case class UntypedLiteralBag(values: Seq[Expr])(implicit val o: Origin) extends CoercingExpr {
  def elementType: Type = Type.leastCommonSuperType(values.map(_.t))
  override def t: Type = TBag(elementType)

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    UntypedLiteralBag(values.map(resolver(_, elementType)))
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

case class Range(from: Expr, to: Expr)(implicit val o: Origin) extends CoercingExpr {
  override def t: Type = TSeq(TInt())
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Range(resolver(from, TInt()), resolver(to, TInt()))
}
case class Values(arr: Expr, from: Expr, to: Expr)(implicit val o: Origin) extends CoercingExpr {
  override def t: Type = TSeq(arr.t.asArray.get.element)

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Values(resolver.array(arr), resolver(from, TInt()), resolver(to, TInt()))
}

case class OptSome(e: Expr)(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TOption(e.t)
}
case class OptNone()(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TOption(TAny())
}

case class MapCons(map: Expr, k: Expr, v: Expr)(implicit val o: Origin) extends CoercingExpr {
  override def t: Type = tailType
  def tailType: TMap = map.t.asMap.get

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    MapCons(resolver.map(map), resolver(k, tailType.key), resolver(v, tailType.value))
}
case class MapEq(left: Expr, right: Expr)(implicit val o: Origin) extends CoercingExpr with BoolExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    MapEq(resolver.map(left), resolver.map(right))
}
case class MapDisjoint(left: Expr, right: Expr)(implicit val o: Origin) extends CoercingExpr with BoolExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    MapDisjoint(resolver.map(left), resolver.map(right))
}
sealed trait MapOp extends CoercingExpr {
  def map: Expr
  def mapType: TMap = map.t.asMap.get
}
case class MapKeySet(map: Expr)(implicit val o: Origin) extends MapOp {
  override def t: Type = TSet(mapType.key)
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    MapKeySet(resolver.map(map))
}
case class MapValueSet(map: Expr)(implicit val o: Origin) extends MapOp {
  override def t: Type = TSet(mapType.value)
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    MapValueSet(resolver.map(map))
}
case class MapItemSet(map: Expr)(implicit val o: Origin) extends MapOp {
  override def t: Type = TSet(TTuple(Seq(mapType.key, mapType.value)))
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    MapItemSet(resolver.map(map))
}
case class MapSize(map: Expr)(implicit val o: Origin) extends MapOp with IntExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    MapSize(resolver.map(map))
}
case class MapRemove(map: Expr, k: Expr)(implicit val o: Origin) extends CoercingExpr {
  def mapType: TMap = map.t.asMap.get
  override def t: Type = mapType

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    MapRemove(resolver.map(map), resolver(k, mapType.key))
}

sealed trait Binder extends Declarator {
  def bindings: Seq[Variable]
  override def declarations: Seq[Declaration] = bindings
}

case class Forall(bindings: Seq[Variable], triggers: Seq[Seq[Expr]], body: Expr)(implicit val o: Origin) extends BoolExpr with Binder with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Forall(bindings, triggers, resolver(body, TBool()))
}
case class Starall(bindings: Seq[Variable], triggers: Seq[Seq[Expr]], body: Expr)(implicit val o: Origin) extends ResourceExpr with Binder with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Starall(bindings, triggers, resolver(body, TResource()))
}
case class Exists(bindings: Seq[Variable], triggers: Seq[Seq[Expr]], body: Expr)(implicit val o: Origin) extends BoolExpr with Binder with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Exists(bindings, triggers, resolver(body, TBool()))
}
case class Sum(bindings: Seq[Variable], condition: Expr, main: Expr)(implicit val o: Origin) extends IntExpr with Binder with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Sum(bindings, resolver(condition, TBool()), resolver(main, TInt()))
}
case class Product(bindings: Seq[Variable], condition: Expr, main: Expr)(implicit val o: Origin) extends IntExpr with Binder with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Product(bindings, resolver(condition, TBool()), resolver(main, TInt()))
}
case class Let(binding: Variable, value: Expr, main: Expr)(implicit val o: Origin) extends Expr with Binder with CoercingExpr {
  override def t: Type = main.t
  override def bindings: Seq[Variable] = Seq(binding)
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Let(binding, resolver(value, binding.t), main)
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
case class DerefPointer(pointer: Expr)(val blame: Blame[PointerDerefError])(implicit val o: Origin) extends CoercingExpr {
  override def t: Type = pointer.t.asPointer.get.element

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    DerefPointer(resolver.pointer(pointer))(blame)
}
case class AddrOf(e: Expr)(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TPointer(e.t)
}

sealed trait Apply extends CoercingExpr {
  def ref: Ref[_ <: Applicable]
  def args: Seq[Expr]

  override def t: Type = ref.decl.returnType

  def coercedArgs(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Seq[Expr] =
    args.zip(ref.decl.args).map {
      case (arg, v) => resolver(arg, v.t)
    }
}

case class PredicateApply(ref: Ref[Predicate], args: Seq[Expr])(implicit val o: Origin) extends Apply {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    PredicateApply(ref, coercedArgs(resolver))
}
case class InstancePredicateApply(obj: Expr, ref: Ref[InstancePredicate], args: Seq[Expr])(implicit val o: Origin) extends Apply {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    InstancePredicateApply(obj, ref, coercedArgs(resolver))
}

case class ADTFunctionInvocation(typeArgs: Option[(Ref[AxiomaticDataType], Seq[Type])],
                                 ref: Ref[ADTFunction], args: Seq[Expr])(implicit val o: Origin) extends Apply {
  override def t: Type =
    typeArgs match {
      case Some((adt, typeArgs)) =>
        ref.decl.returnType.particularize(adt.decl.typeArgs.zip(typeArgs).toMap)
      case None => ref.decl.returnType
    }

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr = {
    typeArgs match {
      case None => ADTFunctionInvocation(typeArgs, ref, coercedArgs(resolver))
      case Some((adt, ts)) =>
        ADTFunctionInvocation(typeArgs, ref,
          ref.decl.args.zip(args).map {
            case (arg, value) => resolver(value, arg.t.particularize(adt.decl.typeArgs.zip(ts).toMap))
          }
        )
    }
  }
}

sealed trait Invocation extends Apply {
  override def ref: Ref[_ <: ContractApplicable]
  def blame: Blame[PreconditionFailed]
  def typeArgs: Seq[Type]

  override def t: Type = super.t.particularize(ref.decl.typeArgs.zip(typeArgs).toMap)

  override def coercedArgs(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Seq[Expr] =
    args.zip(ref.decl.args).map {
      case (arg, v) => resolver(arg, v.t.particularize(ref.decl.typeArgs.zip(typeArgs).toMap))
    }
}

case class ProcedureInvocation(ref: Ref[Procedure], args: Seq[Expr], outArgs: Seq[Ref[Variable]], typeArgs: Seq[Type])
                              (val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends Invocation {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    ProcedureInvocation(ref, coercedArgs(resolver), outArgs, typeArgs)(blame)
}
case class FunctionInvocation(ref: Ref[Function], args: Seq[Expr], typeArgs: Seq[Type])
                             (val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends Invocation {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    FunctionInvocation(ref, coercedArgs(resolver), typeArgs)(blame)
}

case class MethodInvocation(obj: Expr, ref: Ref[InstanceMethod], args: Seq[Expr], outArgs: Seq[Ref[Variable]], typeArgs: Seq[Type])
                           (val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends Invocation {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    MethodInvocation(obj, ref, coercedArgs(resolver), outArgs, typeArgs)(blame)
}
case class InstanceFunctionInvocation(obj: Expr, ref: Ref[InstanceFunction], args: Seq[Expr], typeArgs: Seq[Type])
                                     (val blame: Blame[PreconditionFailed])(implicit val o: Origin) extends Invocation {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    InstanceFunctionInvocation(obj, ref, coercedArgs(resolver), typeArgs)(blame)
}

sealed trait UnExpr extends Expr {
  def arg: Expr
}

case class UMinus(arg: Expr)(implicit val o: Origin) extends UnExpr with CoercingExpr {
  override def t: Type =
    Coercion.getCoercion(arg.t, TInt()) match {
      case Some(_) => TInt()
      case _ => TRational()
    }

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Coercion.getCoercion(arg.t, TInt()) match {
      case Some(_) => UMinus(resolver(arg, TInt()))
      case _ => UMinus(resolver(arg, TRational()))
    }
}
case class BitNot(arg: Expr)(implicit val o: Origin) extends UnExpr with IntExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    BitNot(resolver(arg, TInt()))
}
case class Not(arg: Expr)(implicit val o: Origin) extends UnExpr with BoolExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Not(resolver(arg, TBool()))
}

sealed trait BinExpr extends Expr {
  def left: Expr
  def right: Expr
}
sealed trait NumericBinExpr extends BinExpr with CoercingExpr {
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

case class AmbiguousMult(left: Expr, right: Expr)(implicit val o: Origin) extends CoercingExpr {
  def isProcessOp: Boolean = Coercion.getCoercion(left.t, TProcess()).isDefined
  def isIntOp: Boolean = Coercion.getCoercion(left.t, TInt()).isDefined && Coercion.getCoercion(right.t, TInt()).isDefined

  override def t: Type = if(isProcessOp) TProcess() else (if(isIntOp) TInt() else TRational())

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    if(isProcessOp) AmbiguousMult(resolver(left, TProcess()), resolver(right, TProcess()))
    else if(isIntOp) AmbiguousMult(resolver(left, TInt()), resolver(right, TInt()))
    else AmbiguousMult(resolver(left, TRational()), resolver(right, TRational()))
}

case class AmbiguousPlus(left: Expr, right: Expr)(implicit val o: Origin) extends CoercingExpr {
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
    else if(isSeqOp || isBagOp || isSetOp) Type.leastCommonSuperType(left, right)
    else if(isPointerOp) left.t
    else if(isIntOp) TInt()
    else TRational()

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    if(isProcessOp) AmbiguousPlus(resolver(left, TProcess()), resolver(right, TProcess()))
    else if(isSeqOp) AmbiguousPlus(resolver.seq(left), resolver.seq(right))
    else if(isBagOp) AmbiguousPlus(resolver.bag(left), resolver.bag(right))
    else if(isSetOp) AmbiguousPlus(resolver.set(left), resolver.set(right))
    else if(isPointerOp) AmbiguousPlus(resolver.pointer(left), resolver(right, TInt()))
    else if(isIntOp) AmbiguousPlus(resolver(left, TInt()), resolver(right, TInt()))
    else AmbiguousPlus(resolver(left, TRational()), resolver(right, TRational()))
}


case class AmbiguousOr(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with CoercingExpr {
  def isProcessOp: Boolean = Coercion.getCoercion(left.t, TProcess()).isDefined

  override def t: Type = if(isProcessOp) TProcess() else TBool()

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    if(isProcessOp) AmbiguousOr(resolver(left, TProcess()), resolver(right, TProcess()))
    else AmbiguousOr(resolver(left, TBool()), resolver(right, TBool()))
}

sealed trait BitOp extends BinExpr with CoercingExpr {
  def isBoolOp: Boolean = Coercion.getCoercion(left.t, TBool()).isDefined

  override def t: Type = if(isBoolOp) TBool() else TInt()
}

case class AmbiguousComputationalOr(left: Expr, right: Expr)(implicit val o: Origin) extends BitOp {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    if(isBoolOp) AmbiguousComputationalOr(resolver(left, TBool()), resolver(right, TBool()))
    else AmbiguousComputationalOr(resolver(left, TInt()), resolver(right, TInt()))
}
case class AmbiguousComputationalXor(left: Expr, right: Expr)(implicit val o: Origin) extends BitOp {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    if(isBoolOp) AmbiguousComputationalXor(resolver(left, TBool()), resolver(right, TBool()))
    else AmbiguousComputationalXor(resolver(left, TInt()), resolver(right, TInt()))
}
case class AmbiguousComputationalAnd(left: Expr, right: Expr)(implicit val o: Origin) extends BitOp {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    if(isBoolOp) AmbiguousComputationalAnd(resolver(left, TBool()), resolver(right, TBool()))
    else AmbiguousComputationalAnd(resolver(left, TInt()), resolver(right, TInt()))
}

case class Exp(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    if(isIntOp) Exp(resolver(left, TInt()), resolver(right, TInt()))
    else Exp(resolver(left, TRational()), resolver(right, TRational()))
}
case class Plus(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    if(isIntOp) Plus(resolver(left, TInt()), resolver(right, TInt()))
    else Plus(resolver(left, TRational()), resolver(right, TRational()))
}
case class Minus(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    if(isIntOp) Minus(resolver(left, TInt()), resolver(right, TInt()))
    else Minus(resolver(left, TRational()), resolver(right, TRational()))
}
case class Mult(left: Expr, right: Expr)(implicit val o: Origin) extends NumericBinExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    if(isIntOp) Mult(resolver(left, TInt()), resolver(right, TInt()))
    else Mult(resolver(left, TRational()), resolver(right, TRational()))
}
case class Div(left: Expr, right: Expr)(val blame: Blame[DivByZero])(implicit val o: Origin) extends BinExpr with DividingExpr with CoercingExpr {
  override def t: Type = TRational()
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Div(resolver(left, TRational()), resolver(right, TRational()))(blame)
}
case class FloorDiv(left: Expr, right: Expr)(val blame: Blame[DivByZero])(implicit val o: Origin) extends IntBinExpr with DividingExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    FloorDiv(resolver(left, TRational()), resolver(right, TRational()))(blame)
}
case class Mod(left: Expr, right: Expr)(val blame: Blame[DivByZero])(implicit val o: Origin) extends NumericBinExpr with DividingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr = {
    if(isIntOp) Mod(resolver(left, TInt()), resolver(right, TInt()))(blame)
    else Mod(resolver(left, TRational()), resolver(right, TRational()))(blame)
}
case class BitAnd(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    BitAnd(resolver(left, TInt()), resolver(right, TInt()))
}
case class BitOr(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    BitOr(resolver(left, TInt()), resolver(right, TInt()))
}
case class BitXor(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    BitXor(resolver(left, TInt()), resolver(right, TInt()))
}
case class BitShl(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    BitShl(resolver(left, TInt()), resolver(right, TInt()))
}
// sign-extended (signed)
case class BitShr(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    BitShr(resolver(left, TInt()), resolver(right, TInt()))
}
// not sign-extended (unsigned)
case class BitUShr(left: Expr, right: Expr)(implicit val o: Origin) extends IntBinExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    BitUShr(resolver(left, TInt()), resolver(right, TInt()))
}

object And {
  def fold(exprs: Seq[Expr])(implicit o: Origin): Expr =
    exprs.reduceOption(And(_, _)).getOrElse(Constant.BooleanValue(true))
}
case class And(left: Expr, right: Expr)(implicit val o: Origin) extends BoolBinExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    And(resolver(left, TBool()), resolver(right, TBool()))
}
case class Or(left: Expr, right: Expr)(implicit val o: Origin) extends BoolBinExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Or(resolver(left, TBool()), resolver(right, TBool()))
}
object Implies {
  def unfold(expr: Expr): (Seq[Expr], Expr) = expr match {
    case Implies(left, right) =>
      val (antecedent, consequent) = unfold(right)
      (Star.unfold(left) ++ antecedent, consequent)
    case other => (Nil, other)
  }
}
case class Implies(left: Expr, right: Expr)(implicit val o: Origin) extends BinExpr with CoercingExpr {
  override def t: Type = right.t

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Implies(resolver(left, TBool()), resolver(right, TResource()))
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
case class Star(left: Expr, right: Expr)(implicit val o: Origin) extends ResourceExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Star(resolver(left, TResource()), resolver(right, TResource()))
}
case class Wand(left: Expr, right: Expr)(implicit val o: Origin) extends ResourceExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Wand(resolver(left, TResource()), resolver(right, TResource()))
}
case class Scale(scale: Expr, res: Expr)(implicit val o: Origin) extends ResourceExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Scale(resolver(scale, TRational()), resolver(res, TResource()))
}
case class Unfolding(pred: Expr, body: Expr)(implicit val o: Origin) extends Expr with CoercingExpr {
  override def t: Type = body.t

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Unfolding(resolver(pred, TResource()), body)
}

case class Perm(loc: Expr, perm: Expr)(implicit val o: Origin) extends ResourceExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Perm(loc, resolver(perm, TRational()))
}
case class HPerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends ResourceExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    HPerm(loc, resolver(perm, TRational()))
}
case class APerm(loc: Expr, perm: Expr)(implicit val o: Origin) extends ResourceExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    APerm(loc, resolver(perm, TRational()))
}

case class PointsTo(loc: Expr, perm: Expr, value: Expr)(implicit val o: Origin) extends ResourceExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    PointsTo(loc, resolver(perm, TRational()), resolver(value, loc.t))
}
case class CurPerm(loc: Expr)(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TRational()
}

case class ValidArray(arr: Expr, len: Expr)(implicit val o: Origin) extends BoolExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    ValidArray(resolver.array(arr), resolver(len, TInt()))
}
case class ValidMatrix(mat: Expr, w: Expr, h: Expr)(implicit val o: Origin) extends BoolExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    ValidMatrix(resolver.matrixArray(mat), resolver(w, TInt()), resolver(h, TInt()))
}

case class PermPointer(p: Expr, len: Expr, perm: Expr)(implicit val o: Origin) extends ResourceExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    PermPointer(resolver.pointer(p), resolver(len, TInt()), resolver(perm, TRational()))
}
case class PermPointerIndex(p: Expr, idx: Expr, perm: Expr)(implicit val o: Origin) extends ResourceExpr with CoercingExpr {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    PermPointerIndex(resolver.pointer(p), resolver(idx, TInt()), resolver(perm, TRational()))
}

sealed trait Comparison extends BinExpr {
  override def t: Type = TBool()
}

case class Eq(left: Expr, right: Expr)(implicit val o: Origin) extends Comparison with NoCheck
case class Neq(left: Expr, right: Expr)(implicit val o: Origin) extends Comparison with NoCheck

sealed trait OrderOp extends Comparison with CoercingExpr {
  def isSetOp: Boolean = Coercion.getAnySetCoercion(left.t).isDefined
  def isBagOp: Boolean = Coercion.getAnyBagCoercion(left.t).isDefined
  def isIntOp: Boolean =
    Coercion.getCoercion(left.t, TInt()).isDefined &&
      Coercion.getCoercion(right.t, TInt()).isDefined

  protected def mkCoerced(left: Expr, right: Expr): Expr

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    if(isSetOp) mkCoerced(resolver.set(left), resolver.set(right))
    else if(isBagOp) mkCoerced(resolver.bag(left), resolver.bag(right))
    else if(isIntOp) mkCoerced(resolver(left, TInt()), resolver(right, TInt()))
    else mkCoerced(resolver(left, TResource()), resolver(right, TResource()))
}

case class Greater(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp {
  override protected def mkCoerced(left: Expr, right: Expr): Expr = Greater(left, right)(o)
}
case class Less(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp {
  override protected def mkCoerced(left: Expr, right: Expr): Expr = Less(left, right)(o)
}
case class GreaterEq(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp {
  override protected def mkCoerced(left: Expr, right: Expr): Expr = GreaterEq(left, right)(o)
}
case class LessEq(left: Expr, right: Expr)(implicit val o: Origin) extends OrderOp {
  override protected def mkCoerced(left: Expr, right: Expr): Expr = LessEq(left, right)(o)
}

case class Select(condition: Expr, whenTrue: Expr, whenFalse: Expr)(implicit val o: Origin) extends CoercingExpr {
  override def t: Type =
    Type.leastCommonSuperType(whenTrue.t, whenFalse.t)

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    Select(resolver(condition, TBool()), resolver(whenTrue, t), resolver(whenFalse, t))
}

case class NewObject(cls: Ref[Class])(implicit val o: Origin) extends Expr with NoCheck {
  override def t: Type = TClass(cls)
}

case class NewArray(element: Type, dims: Seq[Expr], moreDims: Int)(implicit val o: Origin) extends CoercingExpr {
  override def t: Type = FuncTools.repeat(TArray(_), dims.size + moreDims, element)

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Expr =
    NewArray(element, dims.map(resolver(_, TInt())), moreDims)
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

case class ArraySubscript(arr: Expr, index: Expr)(val blame: Blame[ArraySubscriptError])(implicit val o: Origin)
  extends Check(arr.checkArrayThen(), index.checkSubType(TInt())) with Expr {
  override def t: Type = arr.t.asArray.get.element
}

case class PointerSubscript(pointer: Expr, index: Expr)(val blame: Blame[PointerSubscriptError])(implicit val o: Origin) extends Check(pointer.checkPointerThen(), index.checkSubType(TInt())) with Expr {
  override def t: Type = pointer.t.asPointer.get.element
}

case class Length(arr: Expr)(val blame: Blame[ArrayNull])(implicit val o: Origin) extends IntExpr {
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
case class BagMemberCount(x: Expr, xs: Expr)(implicit val o: Origin) extends Check(xs.checkBagThen(bag => x.checkSubType(bag.element))) with IntExpr

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
case class OptGet(opt: Expr)(val blame: Blame[OptionNone])(implicit val o: Origin) extends Check(opt.checkOptionThen()) with Expr {
  override def t: Type = opt.t.asOption.get.element
}
case class OptGetOrElse(opt: Expr, alt: Expr)(implicit val o: Origin) extends Check(opt.checkOptionThen(t => alt.checkSubType(t.element))) with Expr {
  override def t: Type = Type.leastCommonSuperType(opt.t.asOption.get.element, alt.t)
}
case class MapGet(map: Expr, k: Expr)(val blame: Blame[MapKeyError])(implicit val o: Origin) extends Check(map.checkMapThen(t => k.checkSubType(t.key))) with Expr {
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

