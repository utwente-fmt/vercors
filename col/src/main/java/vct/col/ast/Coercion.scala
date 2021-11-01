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

  def getCoercion(source: Type, target: Type): Option[Coercion] =
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
}

sealed trait ResolveCoercion {
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
    case Coercion.MapType(source, target, inner) =>
      ???
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
}

case object NopCoercionResolver extends ResolveCoercion

case class Incoercible(expr: Expr, target: Type) extends RuntimeException