package vct.col.typerules

import vct.col.ast._
import vct.col.origin.{DiagnosticOrigin, Origin}

case object CoercionUtils {
  private implicit val o: Origin = DiagnosticOrigin

  def getCoercion[G](source: Type[G], target: Type[G]): Option[Coercion[G]] = {
    Some((source, target) match {
      case (_: TNotAValue[_], _) => return None
      case (_, _: TNotAValue[_]) => return None

      case (source, target) if source == target => return Some(CoerceIdentity(source))
      case (TNothing(), _) => CoerceNothingSomething(target)
      case (_, TAny()) => CoerceSomethingAny(source)

      case (source @ TOption(innerSource), target @ TOption(innerTarget)) =>
        CoerceMapOption(getCoercion(innerSource, innerTarget).getOrElse(return None), innerSource, innerTarget)
      case (source @ TTuple(Seq(leftSource, rightSource)), target @ TTuple(Seq(leftTarget, rightTarget))) =>
        CoerceMapTuple(
          inner = Seq(
            getCoercion(leftSource, leftTarget).getOrElse(return None),
          getCoercion(rightSource, rightTarget).getOrElse(return None),
          ),
          sourceTypes = Seq(leftSource, rightSource),
          targetTypes = Seq(leftTarget, rightTarget),
        )
      case (source @ TEither(leftSource, rightSource), target @ TEither(leftTarget, rightTarget)) =>
        CoerceMapEither(
          (getCoercion(leftSource, leftTarget).getOrElse(return None), getCoercion(rightSource, rightTarget).getOrElse(return None)),
          (leftSource, rightSource),
          (leftTarget, rightTarget),
        )
      case (TSeq(innerSource), TSeq(innerTarget)) =>
        CoerceMapSeq(getCoercion(innerSource, innerTarget).getOrElse(return None), innerSource, innerTarget)
      case (TSet(innerSource), TSet(innerTarget)) =>
        CoerceMapSet(getPromotion(innerSource, innerTarget).getOrElse(return None), innerSource, innerTarget)
      case (TBag(innerSource), TBag(innerTarget)) =>
        CoerceMapBag(getPromotion(innerSource, innerTarget).getOrElse(return None), innerSource, innerTarget)
      case (TMatrix(innerSource), TMatrix(innerTarget)) =>
        CoerceMapMatrix(getCoercion(innerSource, innerTarget).getOrElse(return None), innerSource, innerTarget)
      case (TMap(sourceKey, innerSource), TMap(targetKey, innerTarget)) if sourceKey == targetKey =>
        CoerceMapMap(getCoercion(innerSource, innerTarget).getOrElse(return None), (sourceKey, innerSource), (sourceKey, innerTarget))
      case (TType(innerSource), TType(innerTarget)) =>
        CoerceMapType(getCoercion(innerSource, innerTarget).getOrElse(return None), innerSource, innerTarget)

      case (TNull(), TRef()) => CoerceNullRef()
      case (TNull(), TArray(target)) => CoerceNullArray(target)
      case (TNull(), TClass(target)) => CoerceNullClass(target)
      case (TNull(), JavaTClass(target, _)) => CoerceNullJavaClass(target)
      case (TNull(), TPointer(target)) => CoerceNullPointer(target)

      case (TBool(), TResource()) => CoerceBoolResource()
      case (TFraction(), TZFraction()) => CoerceFracZFrac()
      case (TFraction(), TRational()) => CoercionSequence(Seq(CoerceFracZFrac(), CoerceZFracRat()))
      case (TZFraction(), TRational()) => CoerceZFracRat()
      case (TFloat(_, _), TRational()) => CoerceFloatRat()

      case (source @ TFloat(exponentL, mantissaL), target @ TFloat(exponentR, mantissaR)) if exponentL <= exponentR && mantissaL <= mantissaR =>
        CoerceIncreasePrecision(source, target)

      case (TBoundedInt(gte, lt), TFraction()) if gte >= 1 && lt <= 2 => CoerceBoundIntFrac()
      case (source @ TBoundedInt(gte, lt), TZFraction()) if gte >= 0 && lt <= 2 => CoerceBoundIntZFrac(source)

      case (source @ TBoundedInt(gte, lt), target @ TBoundedInt(t_gte, t_lt)) if t_gte <= gte && t_lt >= lt =>
        CoerceWidenBound(source, target)
      case (source: TBoundedInt[G], TInt()) => CoerceUnboundInt(source)
      case (source: TBoundedInt[G], TRational()) => CoercionSequence(Seq(CoerceUnboundInt(source), CoerceIntRat()))
      case (TInt(), TRational()) => CoerceIntRat()

      case (source @ TClass(sourceClass), target @ TClass(targetClass))
        if source.transSupportArrows.exists { case (_, supp) => supp == targetClass.decl } =>
        CoerceSupports(sourceClass, targetClass)

      case (source @ JavaTClass(sourceClass, Nil), target @ JavaTClass(targetClass, Nil))
        if sourceClass.decl.transSupportArrows(Set.empty).exists { case (_, supp) => supp == targetClass.decl } =>
        CoerceJavaSupports(sourceClass, targetClass)


      case (source @ TUnion(ts), target) =>
        CoerceJoinUnion(ts.map(getCoercion(_, target)).map {
          case Some(coercion) => coercion
          case None => return None
        }, source.types, target)
      case (source, target @ TUnion(ts)) =>
        return ts.map(getCoercion(source, _)).zipWithIndex.collectFirst {
          case (Some(coercion), index) =>
            CoerceSelectUnion(coercion, source, target.types, index)
        }

      case (source @ CPrimitiveType(specs), target) =>
        specs.collectFirst { case spec: CSpecificationType[G] => spec } match {
          case Some(CSpecificationType(t)) =>
            CoercionSequence(Seq(
              CoerceCPrimitiveToCol(source, t),
              getCoercion(t, target).getOrElse(return None),
            ))
          case None => return None
        }

      case (source, target @ CPrimitiveType(specs)) =>
        specs.collectFirst { case spec: CSpecificationType[G] => spec } match {
          case Some(CSpecificationType(t)) =>
            CoercionSequence(Seq(
              getCoercion(source, t).getOrElse(return None),
              CoerceColToCPrimitive(t, target),
            ))
          case None => return None
        }

      // Something with TVar?

      // Unsafe coercions
      case (TRational(), TZFraction()) => CoerceRatZFrac()
      case (TRational(), TFraction()) => CoercionSequence(Seq(CoerceRatZFrac(), CoerceZFracFrac()))
      case (TZFraction(), TFraction()) => CoerceZFracFrac()

      case (_, _) => return None
    })
  }

  def getPromotion[G](source: Type[G], target: Type[G]): Option[Coercion[G]] =
    getCoercion(source, target) match {
      case Some(coercion) if coercion.isPromoting => Some(coercion)
      case _ => None
    }

  def getAnyCCoercion[G](source: Type[G]): Option[(Coercion[G], Type[G])] = source match {
    case t: CPrimitiveType[G] =>
      t.specifiers.collectFirst { case spec: CSpecificationType[G] => spec }.map {
        case CSpecificationType(inner) => (CoerceCPrimitiveToCol(t, inner), inner)
      }
    case _ => None
  }

  def chainCCoercion[G, T](source: CPrimitiveType[G], next: Type[G] => Option[(Coercion[G], T)]): Option[(Coercion[G], T)] =
    getAnyCCoercion(source) match {
      case Some(inner) => next(inner._2) match {
        case Some((coercion, finalType)) =>
          Some((CoercionSequence(Seq(coercion, inner._1)), finalType))
        case None => None
      }
      case None => None
    }

  def getAnySeqCoercion[G](source: Type[G]): Option[(Coercion[G], TSeq[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnySeqCoercion)
    case t: TSeq[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnySetCoercion[G](source: Type[G]): Option[(Coercion[G], TSet[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnySetCoercion)
    case t: TSet[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnyBagCoercion[G](source: Type[G]): Option[(Coercion[G], TBag[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyBagCoercion)
    case t: TBag[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnySizedCoercion[G](source: Type[G]): Option[(Coercion[G], SizedType[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnySizedCoercion)
    case t: TSeq[G] => Some((CoerceIdentity(source), t))
    case t: TSet[G] => Some((CoerceIdentity(source), t))
    case t: TBag[G] => Some((CoerceIdentity(source), t))
    case t: TMap[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnyPointerCoercion[G](source: Type[G]): Option[(Coercion[G], TPointer[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyPointerCoercion)
    case t: TPointer[G] => Some((CoerceIdentity(source), t))
    case _: TNull[G] =>
      val t = TPointer[G](TAny())
      Some((CoerceNullPointer(t), t))
    case _ => None
  }

  def getAnyArrayCoercion[G](source: Type[G]): Option[(Coercion[G], TArray[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyArrayCoercion)
    case t: TArray[G] => Some((CoerceIdentity(source), t))
    case _: TNull[G] =>
      val t = TArray[G](TAny())
      Some((CoerceNullArray(t), t))
    case _ => None
  }

  def getAnyMatrixArrayCoercion[G](source: Type[G]): Option[(Coercion[G], TArray[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyMatrixArrayCoercion)
    case t @ TArray(TArray(_)) => Some((CoerceIdentity(source), t))
    case TArray(TNull()) => Some(???)
    case TNull() =>
      val t = TArray[G](TArray[G](TAny()))
      Some((CoerceNullArray(t), t))
    case _ => None
  }

  def getAnyOptionCoercion[G](source: Type[G]): Option[(Coercion[G], TOption[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyOptionCoercion)
    case t: TOption[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnyMapCoercion[G](source: Type[G]): Option[(Coercion[G], TMap[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyMapCoercion)
    case t: TMap[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnyTupleCoercion[G](source: Type[G]): Option[(Coercion[G], TTuple[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyTupleCoercion)
    case t: TTuple[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnyMatrixCoercion[G](source: Type[G]): Option[(Coercion[G], TMatrix[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyMatrixCoercion)
    case t: TMatrix[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnyModelCoercion[G](source: Type[G]): Option[(Coercion[G], TModel[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyModelCoercion)
    case t: TModel[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnyEitherCoercion[G](source: Type[G]): Option[(Coercion[G], TEither[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyEitherCoercion)
    case t: TEither[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }
  def getAnyClassCoercion[G](source: Type[G]): Option[(Coercion[G], Type[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyClassCoercion)
    case t: TClass[G] => Some((CoerceIdentity(source), t))
    case t: JavaTClass[G] => Some((CoerceIdentity(source), t))
    case t: TUnion[G] =>
      val superType = Types.leastCommonSuperType(t.types)
      getAnyClassCoercion(superType) match {
        case Some((coercion, target)) =>
          val joinedCoercion = CoercionSequence(Seq(
            CoerceJoinUnion(t.types.map(getCoercion(_, superType).get), t.types, superType),
            coercion,
          ))
          Some((joinedCoercion, target))
        case None => None
      }
    case _ => None
  }
}