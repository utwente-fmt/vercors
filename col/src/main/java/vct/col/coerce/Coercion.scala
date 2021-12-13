package vct.col.coerce

import vct.col.ast._

sealed trait Coercion[G] {
  /**
   * Promoting coercions produce a strictly larger type, and must always be implemented injectively.
   */
  def isPromoting: Boolean
}

case object Coercion {
  sealed trait Promotion[G] extends Coercion[G] {
    override def isPromoting: Boolean = true
  }

  sealed trait MappingCoercion[G] extends Coercion[G] {
    def inner: Coercion[G]
    override def isPromoting: Boolean = inner.isPromoting
  }

  sealed trait MappingPromotion[G] extends Promotion[G] with MappingCoercion[G] {
    override def isPromoting: Boolean = true
  }

  case class Identity[G]() extends Promotion[G]
  case class Compose[G](left: Coercion[G], right: Coercion[G]) extends Coercion[G] {
    override def isPromoting: Boolean = left.isPromoting && right.isPromoting
  }

  case class NothingSomething[G](target: Type[G]) extends Promotion[G]
  case class SomethingAny[G]() extends Promotion[G]
  case class MapOption[G](source: TOption[G], target: TOption[G], inner: Coercion[G]) extends MappingCoercion[G]
  case class MapTuple[G](source: TTuple[G], target: TTuple[G], left: Coercion[G], right: Coercion[G]) extends Coercion[G] {
    override def isPromoting: Boolean = left.isPromoting && right.isPromoting
  }
  case class MapEither[G](source: TEither[G], target: TEither[G], left: Coercion[G], right: Coercion[G]) extends Coercion[G] {
    override def isPromoting: Boolean = left.isPromoting && right.isPromoting
  }
  case class MapSeq[G](source: TSeq[G], target: TSeq[G], inner: Coercion[G]) extends MappingCoercion[G]
  case class MapSet[G](source: TSet[G], target: TSet[G], inner: Coercion[G]) extends MappingPromotion[G]
  case class MapBag[G](source: TBag[G], target: TBag[G], inner: Coercion[G]) extends MappingPromotion[G]
  case class MapMatrix[G](source: TMatrix[G], target: TMatrix[G], inner: Coercion[G]) extends MappingCoercion[G]
  case class MapMap[G](source: TMap[G], target: TMap[G], inner: Coercion[G]) extends MappingCoercion[G]
  case class MapType[G](source: TType[G], target: TType[G], inner: Coercion[G]) extends MappingCoercion[G]
  case class Supports[G](source: TClass[G], target: TClass[G]) extends Promotion[G]
  case class NullRef[G]() extends Promotion[G]
  case class NullArray[G](target: TArray[G]) extends Promotion[G]
  case class NullClass[G](target: TClass[G]) extends Promotion[G]
  case class NullJavaClass[G](target: JavaTClass[G]) extends Promotion[G]
  case class NullPointer[G](target: TPointer[G]) extends Promotion[G]
  case class FracZFrac[G]() extends Promotion[G]
  case class ZFracRat[G]() extends Promotion[G]
  case class FloatRat[G](source: TFloat[G]) extends Promotion[G]
  case class WidenBound[G](source: TBoundedInt[G], target: TBoundedInt[G]) extends Promotion[G]
  case class UnboundInt[G](source: TBoundedInt[G]) extends Promotion[G]
  case class BoundIntFrac[G]() extends Promotion[G]
  case class BoundIntZFrac[G](source: TBoundedInt[G]) extends Promotion[G]
  case class IntRat[G]() extends Promotion[G]
  case class BoolResource[G]() extends Promotion[G]
  case class JoinUnion[G](source: TUnion[G], target: Type[G], inner: Seq[Coercion[G]]) extends Coercion[G] {
    override def isPromoting: Boolean = inner.forall(_.isPromoting)
  }
  case class SelectUnion[G](source: Type[G], target: TUnion[G], index: Int, inner: Coercion[G]) extends Coercion[G] {
    override def isPromoting: Boolean = inner.isPromoting
  }

  case class JavaSupports[G](source: JavaTClass[G], target: JavaTClass[G]) extends Promotion[G]

  case class CPrimitiveToCol[G](source: CPrimitiveType[G], target: Type[G]) extends Promotion[G]
  case class ColToCPrimitive[G](source: Type[G], target: CPrimitiveType[G]) extends Promotion[G]

  case class RatZFrac[G]() extends Coercion[G] {
    override def isPromoting: Boolean = false
  }
  case class ZFracFrac[G]() extends Coercion[G] {
    override def isPromoting: Boolean = false
  }

  def getCoercion[G](source: Type[G], target: Type[G]): Option[Coercion[G]] = {
    val result = getCoercion1(source, target)
    result match {
      case Some(Coercion.Identity()) =>
      case Some(Coercion.BoolResource()) =>
      case Some(UnboundInt(_)) =>
      case other =>
        // println(s"$source is a $target by $result")
    }
    result
  }

  def getCoercion1[G](source: Type[G], target: Type[G]): Option[Coercion[G]] =
    Some((source, target) match {
      case (_: TNotAValue[_], _) => return None
      case (_, _: TNotAValue[_]) => return None

      case (source, target) if source == target => Identity()
      case (TNothing(), _) => NothingSomething(target)
      case (_, TAny()) => SomethingAny()

      case (source @ TOption(innerSource), target @ TOption(innerTarget)) =>
        MapOption(source, target, getCoercion(innerSource, innerTarget).getOrElse(return None))
      case (source @ TTuple(Seq(leftSource, rightSource)), target @ TTuple(Seq(leftTarget, rightTarget))) =>
        MapTuple(source, target,
          left = getCoercion(leftSource, leftTarget).getOrElse(return None),
          right = getCoercion(rightSource, rightTarget).getOrElse(return None))
      case (source @ TEither(leftSource, rightSource), target @ TEither(leftTarget, rightTarget)) =>
        MapEither(source, target,
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

      case (TNull(), TRef()) => NullRef()
      case (TNull(), target @ TArray(_)) => NullArray(target)
      case (TNull(), target @ TClass(_)) => NullClass(target)
      case (TNull(), target: JavaTClass[G]) => NullJavaClass(target)
      case (TNull(), target @ TPointer(_)) => NullPointer(target)

      case (TBool(), TResource()) => BoolResource()
      case (TFraction(), TZFraction()) => FracZFrac()
      case (TFraction(), TRational()) => Compose(ZFracRat(), FracZFrac())
      case (TZFraction(), TRational()) => ZFracRat()
      case (source: TFloat[G], TRational()) => FloatRat(source)

      case (TBoundedInt(gte, lt), TFraction()) if gte >= 1 && lt <= 2 => BoundIntFrac()
      case (source @ TBoundedInt(gte, lt), TZFraction()) if gte >= 0 && lt <= 2 => BoundIntZFrac(source)

      case (source @ TBoundedInt(gte, lt), target @ TBoundedInt(t_gte, t_lt)) if t_gte <= gte && t_lt >= lt =>
        WidenBound(source, target)
      case (source: TBoundedInt[G], TInt()) => UnboundInt(source)
      case (source: TBoundedInt[G], TRational()) => Compose(IntRat(), UnboundInt(source))
      case (TInt(), TRational()) => IntRat()

      case (source: TClass[G], target: TClass[G])
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

      case (source @ CPrimitiveType(specs), target) =>
        specs.collectFirst { case spec: CSpecificationType[G] => spec } match {
          case Some(CSpecificationType(t)) =>
            Compose(
              getCoercion(t, target).getOrElse(return None),
              CPrimitiveToCol(source, t)
            )
          case None => return None
        }

      case (source, target @ CPrimitiveType(specs)) =>
        specs.collectFirst { case spec: CSpecificationType[G] => spec } match {
          case Some(CSpecificationType(t)) =>
            Compose(
              ColToCPrimitive(t, target),
              getCoercion(source, t).getOrElse(return None),
            )
          case None => return None
        }

      // Something with TVar?

      // Unsafe coercions
      case (TRational(), TZFraction()) => RatZFrac()
      case (TRational(), TFraction()) => Compose(ZFracFrac(), RatZFrac())
      case (TZFraction(), TFraction()) => ZFracFrac()

      case (source, target) => return None
    })

  def getPromotion[G](source: Type[G], target: Type[G]): Option[Coercion[G]] =
    getCoercion(source, target) match {
      case Some(coercion) if coercion.isPromoting => Some(coercion)
      case _ => None
    }

  def getAnyCCoercion[G](source: Type[G]): Option[(Coercion[G], Type[G])] = source match {
    case t: CPrimitiveType[G] =>
      t.specifiers.collectFirst { case spec: CSpecificationType[G] => spec }.map {
        case CSpecificationType(inner) => (CPrimitiveToCol(t, inner), inner)
      }
    case _ => None
  }

  def chainCCoercion[G, T](source: CPrimitiveType[G], next: Type[G] => Option[(Coercion[G], T)]): Option[(Coercion[G], T)] =
    getAnyCCoercion(source) match {
      case Some(inner) => next(inner._2) match {
        case Some((coercion, finalType)) =>
          Some((Compose(coercion, inner._1), finalType))
        case None => None
      }
      case None => None
    }

  def getAnySeqCoercion[G](source: Type[G]): Option[(Coercion[G], TSeq[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnySeqCoercion)
    case t: TSeq[G] => Some((Identity(), t))
    case _ => None
  }

  def getAnySetCoercion[G](source: Type[G]): Option[(Coercion[G], TSet[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnySetCoercion)
    case t: TSet[G] => Some((Identity(), t))
    case _ => None
  }

  def getAnyBagCoercion[G](source: Type[G]): Option[(Coercion[G], TBag[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyBagCoercion)
    case t: TBag[G] => Some((Identity(), t))
    case _ => None
  }

  def getAnyCollectionCoercion[G](source: Type[G]): Option[(Coercion[G], SizedType[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyCollectionCoercion)
    case t: TSeq[G] => Some((Identity(), t))
    case t: TSet[G] => Some((Identity(), t))
    case t: TBag[G] => Some((Identity(), t))
    case _ => None
  }

  def getAnyPointerCoercion[G](source: Type[G]): Option[(Coercion[G], TPointer[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyPointerCoercion)
    case t: TPointer[G] => Some((Identity(), t))
    case _: TNull[G] =>
      val t = TPointer[G](TAny())
      Some((NullPointer(t), t))
    case _ => None
  }

  def getAnyArrayCoercion[G](source: Type[G]): Option[(Coercion[G], TArray[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyArrayCoercion)
    case t: TArray[G] => Some((Identity(), t))
    case _: TNull[G] =>
      val t = TArray[G](TAny())
      Some((NullArray(t), t))
    case _ => None
  }

  def getAnyMatrixArrayCoercion[G](source: Type[G]): Option[(Coercion[G], TArray[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyMatrixArrayCoercion)
    case t @ TArray(TArray(_)) => Some((Identity(), t))
    case TArray(TNull()) => Some(???)
    case TNull() =>
      val t = TArray[G](TArray[G](TAny()))
      Some((NullArray(t), t))
    case _ => None
  }

  def getAnyOptionCoercion[G](source: Type[G]): Option[(Coercion[G], TOption[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyOptionCoercion)
    case t: TOption[G] => Some((Identity(), t))
    case _ => None
  }

  def getAnyMapCoercion[G](source: Type[G]): Option[(Coercion[G], TMap[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyMapCoercion)
    case t: TMap[G] => Some((Identity(), t))
    case _ => None
  }

  def getAnyTupleCoercion[G](source: Type[G]): Option[(Coercion[G], TTuple[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyTupleCoercion)
    case t: TTuple[G] => Some((Identity(), t))
    case _ => None
  }

  def getAnyMatrixCoercion[G](source: Type[G]): Option[(Coercion[G], TMatrix[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyMatrixCoercion)
    case t: TMatrix[G] => Some((Identity(), t))
    case _ => None
  }

  def getAnyModelCoercion[G](source: Type[G]): Option[(Coercion[G], TModel[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyModelCoercion)
    case t: TModel[G] => Some((Identity(), t))
    case _ => None
  }

  def getAnyEitherCoercion[G](source: Type[G]): Option[(Coercion[G], TEither[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyEitherCoercion)
    case t: TEither[G] => Some((Identity(), t))
    case _ => None
  }
  def getAnyClassCoercion[G](source: Type[G]): Option[(Coercion[G], TClass[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyClassCoercion)
    case t: TClass[G] => Some((Identity(), t))
    case _ => None
  }
}