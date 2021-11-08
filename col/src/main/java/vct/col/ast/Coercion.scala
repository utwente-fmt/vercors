package vct.col.ast

import vct.col.ast.AstBuildHelpers._

import scala.math.BigInt

sealed trait Coercion {
  /**
   * Promoting coercions produce a strictly larger type, and must always be implemented injectively.
   */
  def isPromoting: Boolean
}

case object Coercion {
  sealed trait Promotion extends Coercion {
    override def isPromoting: Boolean = true
  }

  sealed trait MappingCoercion extends Coercion {
    def inner: Coercion
    override def isPromoting: Boolean = inner.isPromoting
  }

  sealed trait MappingPromotion extends Promotion with MappingCoercion {
    override def isPromoting: Boolean = true
  }

  case object Identity extends Promotion
  case class Compose(left: Coercion, right: Coercion) extends Coercion {
    override def isPromoting: Boolean = left.isPromoting && right.isPromoting
  }

  case class NothingSomething(target: Type) extends Promotion
  case object SomethingAny extends Promotion
  case class MapOption(source: TOption, target: TOption, inner: Coercion) extends MappingCoercion
  case class MapTuple(source: TTuple, target: TTuple, left: Coercion, right: Coercion) extends Coercion {
    override def isPromoting: Boolean = left.isPromoting && right.isPromoting
  }
  case class MapSeq(source: TSeq, target: TSeq, inner: Coercion) extends MappingCoercion
  case class MapSet(source: TSet, target: TSet, inner: Coercion) extends MappingPromotion
  case class MapBag(source: TBag, target: TBag, inner: Coercion) extends MappingPromotion
  case class MapMatrix(source: TMatrix, target: TMatrix, inner: Coercion) extends MappingCoercion
  case class MapMap(source: TMap, target: TMap, inner: Coercion) extends MappingCoercion
  case class MapType(source: TType, target: TType, inner: Coercion) extends MappingCoercion
  case class Supports(source: TClass, target: TClass) extends Promotion
  case class JavaSupports(source: JavaTClass, target: JavaTClass) extends Promotion
  case object NullRef extends Promotion
  case class NullArray(target: TArray) extends Promotion
  case class NullClass(target: TClass) extends Promotion
  case class NullJavaClass(target: JavaTClass) extends Promotion
  case class NullPointer(target: TPointer) extends Promotion
  case object FracZFrac extends Promotion
  case object ZFracRat extends Promotion
  case class FloatRat(source: TFloat) extends Promotion
  case class WidenBound(source: TBoundedInt, target: TBoundedInt) extends Promotion
  case class UnboundInt(source: TBoundedInt) extends Promotion
  case object BoundIntFrac extends Promotion
  case class BoundIntZFrac(source: TBoundedInt) extends Promotion
  case object IntRat extends Promotion
  case object BoolResource extends Promotion
  case class JoinUnion(source: TUnion, target: Type, inner: Seq[Coercion]) extends Coercion {
    override def isPromoting: Boolean = inner.forall(_.isPromoting)
  }
  case class SelectUnion(source: Type, target: TUnion, index: Int, inner: Coercion) extends Coercion {
    override def isPromoting: Boolean = inner.isPromoting
  }

  case object RatZFrac extends Coercion {
    override def isPromoting: Boolean = false
  }
  case object ZFracFrac extends Coercion {
    override def isPromoting: Boolean = false
  }

  def getCoercion(source: Type, target: Type): Option[Coercion] = {
    val result = getCoercion1(source, target)
    result match {
      case Some(Coercion.Identity) =>
      case Some(Coercion.BoolResource) =>
      case Some(UnboundInt(_)) =>
      case other =>
        println(s"$source is a $target by $result")
    }
    result
  }

  def getCoercion1(source: Type, target: Type): Option[Coercion] =
    Some((source, target) match {
      case (TNotAValue(), _) => return None
      case (_, TNotAValue()) => return None

      case (source, target) if source == target => Identity
      case (TNothing(), _) => NothingSomething(target)
      case (_, TAny()) => SomethingAny

      case (source @ TOption(innerSource), target @ TOption(innerTarget)) =>
        MapOption(source, target, getCoercion(innerSource, innerTarget).getOrElse(return None))
      case (source @ TTuple(Seq(leftSource, rightSource)), target @ TTuple(Seq(leftTarget, rightTarget))) =>
        MapTuple(source, target,
          left = getCoercion(leftSource, leftTarget).getOrElse(return None),
          right = getCoercion(rightSource, rightTarget).getOrElse(return None))
      case (source @ TSeq(innerSource), target @ TSeq(innerTarget)) =>
        MapSeq(source, target, getCoercion(innerSource, innerTarget).getOrElse(return None))
      case (source @ TSet(innerSource), target @ TSet(innerTarget)) =>
        MapSet(source, target, getPromotion(innerSource, innerTarget).getOrElse(return None))
      case (source @ TBag(innerSource), target @ TBag(innerTarget)) =>
        MapBag(source, target, getPromotion(innerSource, innerTarget).getOrElse(return None))
      case (source @ TMatrix(innerSource), target @ TMatrix(innerTarget)) =>
        MapMatrix(source, target, getCoercion(innerSource, innerTarget).getOrElse(return None))
      case (source @ TMap(sourceKey, innerSource), target @ TMap(targetKey, innerTarget)) if sourceKey == targetKey =>
        MapMap(source, target, getCoercion(innerSource, innerTarget).getOrElse(return None))
      case (source @ TType(innerSource), target @ TType(innerTarget)) =>
        MapType(source, target, getCoercion(innerSource, innerTarget).getOrElse(return None))

      case (TNull(), TRef()) => NullRef
      case (TNull(), target @ TArray(_)) => NullArray(target)
      case (TNull(), target @ TClass(_)) => NullClass(target)
      case (TNull(), target: JavaTClass) => NullJavaClass(target)
      case (TNull(), target @ TPointer(_)) => NullPointer(target)

      case (TBool(), TResource()) => BoolResource
      case (TFraction(), TZFraction()) => FracZFrac
      case (TFraction(), TRational()) => Compose(ZFracRat, FracZFrac)
      case (TZFraction(), TRational()) => ZFracRat
      case (source: TFloat, TRational()) => FloatRat(source)

      case (TBoundedInt(gte, lt), TFraction()) if gte >= 1 && lt <= 2 => BoundIntFrac
      case (source @ TBoundedInt(gte, lt), TZFraction()) if gte >= 0 && lt <= 2 => BoundIntZFrac(source)

      case (source @ TBoundedInt(gte, lt), target @ TBoundedInt(t_gte, t_lt)) if t_gte <= gte && t_lt >= lt =>
        WidenBound(source, target)
      case (source: TBoundedInt, TInt()) => UnboundInt(source)
      case (source: TBoundedInt, TRational()) => Compose(IntRat, UnboundInt(source))
      case (TInt(), TRational()) => IntRat

      case (source: TClass, target: TClass)
        if source.transSupportArrows.exists { case (_, supp) => supp == target.cls.decl } =>
        Supports(source, target)

      case (source @ TUnion(ts), target) =>
        JoinUnion(source, target, ts.map(getCoercion(_, target)).map {
          case Some(coercion) => coercion
          case None => return None
        })
      case (source, target @ TUnion(ts)) =>
        return ts.map(getCoercion(source, _)).zipWithIndex.collectFirst {
          case (Some(coercion), index) =>
            SelectUnion(source, target, index, coercion)
        }

      // Something with TVar?

      // Unsafe coercions
      case (TRational(), TZFraction()) => RatZFrac
      case (TRational(), TFraction()) => Compose(ZFracFrac, RatZFrac)
      case (TZFraction(), TFraction()) => ZFracFrac

      case (_, _) => return None
    })

  def getPromotion(source: Type, target: Type): Option[Coercion] =
    getCoercion(source, target) match {
      case Some(coercion) if coercion.isPromoting => Some(coercion)
      case _ => None
    }

  def getAnySeqCoercion(source: Type): Option[(Coercion, TSeq)] = source match {
    case t: TSeq => Some((Identity, t))
    case _ => None
  }

  def getAnySetCoercion(source: Type): Option[(Coercion, TSet)] = source match {
    case t: TSet => Some((Identity, t))
    case _ => None
  }

  def getAnyBagCoercion(source: Type): Option[(Coercion, TBag)] = source match {
    case t: TBag => Some((Identity, t))
    case _ => None
  }

  def getAnyCollectionCoercion(source: Type): Option[(Coercion, CollectionType)] = source match {
    case t: TSeq => Some((Identity, t))
    case t: TSet => Some((Identity, t))
    case t: TBag => Some((Identity, t))
    case _ => None
  }

  def getAnyPointerCoercion(source: Type): Option[(Coercion, TPointer)] = source match {
    case t: TPointer => Some((Identity, t))
    case _: TNull =>
      val t = TPointer(TAny())
      Some((NullPointer(t), t))
    case _ => None
  }

  def getAnyArrayCoercion(source: Type): Option[(Coercion, TArray)] = source match {
    case t: TArray => Some((Identity, t))
    case _: TNull =>
      val t = TArray(TAny())
      Some((NullArray(t), t))
    case _ => None
  }

  def getAnyMatrixArrayCoercion(source: Type): Option[(Coercion, TArray)] = source match {
    case t @ TArray(TArray(_)) => Some((Identity, t))
    case TArray(TNull()) => Some(???)
    case TNull() =>
      val t = TArray(TArray(TAny()))
      Some((NullArray(t), t))
    case _ => None
  }

  def getAnyOptionCoercion(source: Type): Option[(Coercion, TOption)] = source match {
    case t: TOption => Some((Identity, t))
    case _ => None
  }

  def getAnyMapCoercion(source: Type): Option[(Coercion, TMap)] = source match {
    case t: TMap => Some((Identity, t))
    case _ => None
  }

  def getAnyTupleCoercion(source: Type): Option[(Coercion, TTuple)] = source match {
    case t: TTuple => Some((Identity, t))
    case _ => None
  }

  def getAnyMatrixCoercion(source: Type): Option[(Coercion, TMatrix)] = source match {
    case t: TMatrix => Some((Identity, t))
    case _ => None
  }

  def getAnyModelCoercion(source: Type): Option[(Coercion, TModel)] = source match {
    case t: TModel => Some((Identity, t))
    case _ => None
  }

  def getAnyClassCoercion(source: Type): Option[(Coercion, TClass)] = source match {
    case t: TClass => Some((Identity, t))
    case _ => None
  }
}

trait ResolveCoercion {
  /**
   * Apply a particular coercion to an expression.
   * SAFETY: all promoting coercions must be injective; otherwise the default mapping coercion of sets is unsound.
   * @param e the expression to coerce
   * @param coercion the coercion
   * @param o the origin to assign to generated expressions
   * @param sc the scope in which to declare functions
   * @return the coerced expression
   */
  def apply(e: Expr, coercion: Coercion)(implicit o: Origin, sc: ScopeContext): Expr = coercion match {
    case Coercion.Identity => e
    case Coercion.Compose(left, right) => apply(apply(e, right), left)
    case Coercion.NothingSomething(_) => e
    case Coercion.SomethingAny => e
    case Coercion.MapOption(_, _, inner) =>
      Select(Eq(e, OptNone()), OptNone(), apply(OptGet(e)(NeverNone), inner))
    case Coercion.MapSeq(source, target, inner) =>
      val result = AmbiguousResult()
      result.ref = Some(target)
      val v = new Variable(source)
      val i = new Variable(TInt())
      val v_i = SeqSubscript(v.get, i.get)(FramedSeqIndex)
      val result_i = SeqSubscript(result, i.get)(FramedSeqIndex)

      val f = function(
        blame = AbstractApplicable,
        returnType = target,
        args = Seq(v),
        ensures =
          Eq(Size(v.get), Size(result)) &&
            Forall(Seq(i), Seq(Seq(result_i)),
              (const(0) < i.get && i.get < Size(result)) ==>
                result_i === apply(v_i, inner)),
      )

      f.declareDefault(sc)
      FunctionInvocation(f.ref, Seq(e), Nil)(PanicBlame("default coercion for seq<_> requires nothing."))
    case Coercion.MapSet(source, target, inner) =>
      val result = AmbiguousResult()
      result.ref = Some(target)
      val v = new Variable(source)
      val elem = new Variable(source.element)

      val f = function(
        blame = AbstractApplicable,
        returnType = target,
        args = Seq(v),
        ensures =
          Eq(Size(result), Size(v.get)) &&
            Forall(Seq(elem), Seq(Seq(SetMember(elem.get, result))),
              Eq(SetMember(apply(elem.get, inner), result), SetMember(elem.get, v.get)))
      )

      f.declareDefault(sc)
      FunctionInvocation(f.ref, Seq(e), Nil)(PanicBlame("Default coercion for set<_> requires nothing."))
    case Coercion.MapBag(source, target, inner) =>
      val result = AmbiguousResult()
      result.ref = Some(target)
      val v = new Variable(source)
      val elem = new Variable(source.element)

      val f = function(
        blame = AbstractApplicable,
        returnType = target,
        args = Seq(v),
        ensures =
          Eq(Size(result), Size(v.get)) &&
            Forall(Seq(elem), Seq(Seq(BagMemberCount(elem.get, result))),
              Eq(BagMemberCount(apply(elem.get, inner), result), BagMemberCount(elem.get, v.get)))
      )

      f.declareDefault(sc)
      FunctionInvocation(f.ref, Seq(e), Nil)(PanicBlame("Default coercion for bag<_> requires nothing."))
    case Coercion.MapMatrix(source, target, inner) =>
      ???
    case Coercion.MapMap(source, target, inner) =>
      val result = AmbiguousResult()
      result.ref = Some(target)
      val v = new Variable(source)
      val k = new Variable(source.key)

      val f = function(
        blame = AbstractApplicable,
        returnType = target,
        args = Seq(v),
        ensures =
          Eq(MapKeySet(result), MapKeySet(v.get)) &&
            Forall(Seq(k), Seq(Seq(MapGet(result, k.get)(TriggerPatternBlame))),
              SetMember(k.get, MapKeySet(result)) ==> Eq(MapGet(result, k.get)(FramedMapGet), MapGet(v.get, k.get)(FramedMapGet)))
      )

      f.declareDefault(sc)
      FunctionInvocation(f.ref, Seq(e), Nil)(PanicBlame("Default coercion for map<_, _> requires nothing."))
    case Coercion.MapTuple(source, target, left, right) =>
      LiteralTuple(target.elements, Seq(apply(TupGet(e, 0), left), apply(TupGet(e, 1), right)))
    case Coercion.MapType(source, target, inner) =>
      ???

    case Coercion.BoolResource => e
    case Coercion.BoundIntFrac => e
    case Coercion.BoundIntZFrac(_) => e
    case Coercion.JoinUnion(_, _, inner) => e
    case Coercion.SelectUnion(_, _, _, inner) => apply(e, inner)

    case Coercion.Supports(_, _) => e
    case Coercion.JavaSupports(_, _) => e
    case Coercion.NullRef => e
    case Coercion.NullArray(_) => e
    case Coercion.NullClass(_) => e
    case Coercion.NullJavaClass(_) => e
    case Coercion.NullPointer(_) => e
    case Coercion.FracZFrac => e
    case Coercion.ZFracRat => e
    case Coercion.FloatRat(_) => e
    case Coercion.WidenBound(_, _) => e
    case Coercion.UnboundInt(_) => e

    case Coercion.IntRat => e
    case Coercion.RatZFrac => e
    case Coercion.ZFracFrac => e
  }

  def apply(e: Expr, target: Type)(implicit o: Origin, sc: ScopeContext): Expr =
    apply(e, Coercion.getCoercion(e.t, target) match {
      case Some(coercion) => coercion
      case None => throw Incoercible(e, target)
    })

  def seq(e: Expr)(implicit o: Origin, sc: ScopeContext): Expr =
    apply(e, Coercion.getAnySeqCoercion(e.t) match {
      case Some(coercion) => coercion
      case None => throw Incoercible(e, TSeq(TAny()))
    })

  def set(e: Expr)(implicit o: Origin, sc: ScopeContext): Expr =
    apply(e, Coercion.getAnySetCoercion(e.t) match {
      case Some(coercion) => coercion
      case None => throw Incoercible(e, TSet(TAny()))
    })

  def bag(e: Expr)(implicit o: Origin, sc: ScopeContext): Expr =
    apply(e, Coercion.getAnyBagCoercion(e.t) match {
      case Some(coercion) => coercion
      case None => throw Incoercible(e, TBag(TAny()))
    })

  def collection(e: Expr)(implicit o: Origin, sc: ScopeContext): Expr =
    apply(e, Coercion.getAnyCollectionCoercion(e.t) match {
      case Some(coercion) => coercion
      case None => throw IncoercibleText(e, got => s"Expected a collection type (sequence, set or bag) but got $got")
    })

  def pointer(e: Expr)(implicit o: Origin, sc: ScopeContext): Expr =
    apply(e, Coercion.getAnyPointerCoercion(e.t) match {
      case Some(coercion) => coercion
      case None => throw Incoercible(e, TPointer(TAny()))
    })

  def array(e: Expr)(implicit o: Origin, sc: ScopeContext): Expr =
    apply(e, Coercion.getAnyArrayCoercion(e.t) match {
      case Some(coercion) => coercion
      case None => throw Incoercible(e, TArray(TAny()))
    })

  def matrixArray(e: Expr)(implicit o: Origin, sc: ScopeContext): Expr =
    apply(e, Coercion.getAnyMatrixArrayCoercion(e.t) match {
      case Some(coercion) => coercion
      case None => throw IncoercibleText(e, got => s"Expected a two-dimensional array, but got $got")
    })

  def option(e: Expr)(implicit o: Origin, sc: ScopeContext): Expr =
    apply(e, Coercion.getAnyOptionCoercion(e.t) match {
      case Some(coercion) => coercion
      case None => throw Incoercible(e, TOption(TAny()))
    })

  def map(e: Expr)(implicit o: Origin, sc: ScopeContext): Expr =
    apply(e, Coercion.getAnyMapCoercion(e.t) match {
      case Some(coercion) => coercion
      case None => throw Incoercible(e, TMap(TAny(), TAny()))
    })

  def tuple(e: Expr)(implicit o: Origin, sc: ScopeContext): Expr =
    apply(e, Coercion.getAnyTupleCoercion(e.t) match {
      case Some(coercion) => coercion
      case None => throw Incoercible(e, TTuple(Seq(TAny(), TAny())))
    })

  def matrix(e: Expr)(implicit o: Origin, sc: ScopeContext): Expr =
    apply(e, Coercion.getAnyMatrixCoercion(e.t) match {
      case Some(coercion) => coercion
      case None => throw Incoercible(e, TMatrix(TAny()))
    })

  def model(e: Expr)(implicit o: Origin, sc: ScopeContext): Expr =
    apply(e, Coercion.getAnyModelCoercion(e.t) match {
      case Some(coercion) => coercion
      case None => throw IncoercibleText(e, got => s"Expected a model type, but got $got")
    })

  def cls(e: Expr)(implicit o: Origin, sc: ScopeContext): Expr =
    apply(e, Coercion.getAnyClassCoercion(e.t) match {
      case Some(coercion) => coercion
      case None => throw IncoercibleText(e, got => s"Expected a class type, but got $got")
    })
}

case object NopCoercionResolver extends ResolveCoercion

case class Incoercible(expr: Expr, target: Type) extends RuntimeException
case class IncoercibleText(expr: Expr, message: Type => String) extends RuntimeException