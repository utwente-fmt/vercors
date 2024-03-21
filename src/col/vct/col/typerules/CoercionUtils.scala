package vct.col.typerules

import hre.util.FuncTools
import vct.col.ast._
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.resolve.lang.{C, CPP}
import vct.col.resolve.lang.CPP.getBaseTypeFromSpecs

case object CoercionUtils {
  private implicit val o: Origin = DiagnosticOrigin

  // We only want Coercions that are promoting in C
  def getCoercion[G](source: Type[G], target: Type[G]): Option[Coercion[G]] =
    getAnyCoercion(source, target).filter(_.isCPromoting)

  def getAnyCoercion[G](source: Type[G], target: Type[G]): Option[Coercion[G]] = {
    Some((source, target) match {
      case (_: TNotAValue[_], _) => return None
      case (_, _: TNotAValue[_]) => return None

      case (source, target) if source == target => return Some(CoerceIdentity(source))
      case (TNothing(), _) => CoerceNothingSomething(target)
      case (_, TAny()) => CoerceSomethingAny(source)

      case (TResource(), TAnyValue()) => CoercionSequence(Seq(CoerceResourceResourceVal(), CoerceSomethingAnyValue(TResourceVal())))
      case (TResource(), TResourceVal()) => CoerceResourceResourceVal()
      case (TResourceVal(), TResource()) => CoerceResourceValResource()
      case (TBool(), TResource()) => CoerceBoolResource()
      case (TBool(), TResourceVal()) => CoercionSequence(Seq(CoerceBoolResource(), CoerceResourceResourceVal()))

      case (_, TAnyValue()) => CoerceSomethingAnyValue(source)

      case (source @ TOption(innerSource), target @ TOption(innerTarget)) =>
        CoerceMapOption(getAnyCoercion(innerSource, innerTarget).getOrElse(return None), innerSource, innerTarget)
      case (source @ TTuple(Seq(leftSource, rightSource)), target @ TTuple(Seq(leftTarget, rightTarget))) =>
        CoerceMapTuple(
          inner = Seq(
            getAnyCoercion(leftSource, leftTarget).getOrElse(return None),
            getAnyCoercion(rightSource, rightTarget).getOrElse(return None),
          ),
          sourceTypes = Seq(leftSource, rightSource),
          targetTypes = Seq(leftTarget, rightTarget),
        )
      case (source @ TEither(leftSource, rightSource), target @ TEither(leftTarget, rightTarget)) =>
        CoerceMapEither(
          (getAnyCoercion(leftSource, leftTarget).getOrElse(return None), getAnyCoercion(rightSource, rightTarget).getOrElse(return None)),
          (leftSource, rightSource),
          (leftTarget, rightTarget),
        )
      case (TSeq(innerSource), TSeq(innerTarget)) =>
        CoerceMapSeq(getAnyCoercion(innerSource, innerTarget).getOrElse(return None), innerSource, innerTarget)
      case (TSet(innerSource), TSet(innerTarget)) =>
        CoerceMapSet(getPromotion(innerSource, innerTarget).getOrElse(return None), innerSource, innerTarget)
      case (TBag(innerSource), TBag(innerTarget)) =>
        CoerceMapBag(getPromotion(innerSource, innerTarget).getOrElse(return None), innerSource, innerTarget)
      case (TMatrix(innerSource), TMatrix(innerTarget)) =>
        CoerceMapMatrix(getAnyCoercion(innerSource, innerTarget).getOrElse(return None), innerSource, innerTarget)
      case (TMap(sourceKey, innerSource), TMap(targetKey, innerTarget)) if sourceKey == targetKey =>
        CoerceMapMap(getAnyCoercion(innerSource, innerTarget).getOrElse(return None), (sourceKey, innerSource), (sourceKey, innerTarget))
      case (TType(innerSource), TType(innerTarget)) =>
        CoerceMapType(getAnyCoercion(innerSource, innerTarget).getOrElse(return None), innerSource, innerTarget)

      case (TNull(), TRef()) => CoerceNullRef()
      case (TNull(), TArray(target)) => CoerceNullArray(target)
      case (TNull(), TClass(target)) => CoerceNullClass(target)
      case (TNull(), JavaTClass(target, _)) => CoerceNullJavaClass(target)
      case (TNull(), TAnyClass()) => CoerceNullAnyClass()
      case (TNull(), TPointer(target)) => CoerceNullPointer(target)
      case (TNull(), CTPointer(target)) => CoerceNullPointer(target)
      case (TNull(), TEnum(target)) => CoerceNullEnum(target)

      case (CTArray(_, innerType), TArray(element)) if element == innerType =>
        CoerceCArrayPointer(element)
      case (CPPTArray(_, innerType), TArray(element)) if element == innerType =>
        CoerceCPPArrayPointer(element)
      case (CTPointer(innerType), TPointer(element)) => //if element == innerType =>
        getAnyCoercion(element, innerType).getOrElse(return None)
      case (TPointer(element), CTPointer(innerType)) => //if element == innerType =>
        getAnyCoercion(element, innerType).getOrElse(return None)
      case (CTArray(_, innerType), CTPointer(element)) =>
        if(element == innerType){
          CoerceCArrayPointer(innerType)
        } else {
          CoercionSequence(Seq(
            CoerceCArrayPointer(element),
            getAnyCoercion(element, innerType).getOrElse(return None)
          ))
        }
      case (TFraction(), TZFraction()) => CoerceFracZFrac()
      case (TFraction(), TRational()) => CoercionSequence(Seq(CoerceFracZFrac(), CoerceZFracRat()))
      case (TZFraction(), TRational()) => CoerceZFracRat()
      case (source: FloatType[G], TRational()) => CoerceFloatRat(source)
      case (TBool(), TCInt()) => CoerceBoolCInt()
      case (TCInt(), TBool()) => CoerceCIntBool()
      case (TCInt(), TResource()) => CoerceCIntBool()
      case (TPointer(_), TBool()) => CoercePointerBool()
      case (CTPointer(_), TBool()) => CoercePointerBool()

      case (source @ TFloat(exponentL, mantissaL), target @ TFloat(exponentR, mantissaR)) if exponentL <= exponentR && mantissaL <= mantissaR =>
        CoerceIncreasePrecision(source, target)
      case (source@TCFloat(exponentL, mantissaL), target@TCFloat(exponentR, mantissaR)) if exponentL <= exponentR && mantissaL <= mantissaR =>
        CoerceIncreasePrecision(source, target)
      case (source@TCFloat(_, _), target@TCFloat(_, _)) =>
        CoerceDecreasePrecision(source, target)
      // Allow to go from C float to normal float and C Int to Int
      case (source@TCFloat(exponentL, mantissaL), target@TFloat(exponentR, mantissaR)) if exponentL == exponentR && mantissaL == mantissaR =>
        CoerceCFloatFloat(source, target)
      case (source@TCFloat(exponentL, mantissaL), target@TFloat(exponentR, mantissaR)) if exponentL < exponentR && mantissaL < mantissaR =>
        val coercedCFloat = TCFloat[G](exponentR, mantissaR)
        CoercionSequence(Seq(CoerceIncreasePrecision(source, coercedCFloat), CoerceCFloatFloat(coercedCFloat, target)))
      case (source@TCFloat(_, _), target@TFloat(exponentR, mantissaR)) =>
        val coercedCFloat = TCFloat[G](exponentR, mantissaR)
        CoercionSequence(Seq(CoerceDecreasePrecision(source, coercedCFloat), CoerceCFloatFloat(coercedCFloat, target)))
      case (TCInt(), TInt()) => CoerceCIntInt()

      case (TBoundedInt(gte, lt), TFraction()) if gte >= 1 && lt <= 2 => CoerceBoundIntFrac()
      case (source @ TBoundedInt(gte, lt), TZFraction()) if gte >= 0 && lt <= 2 => CoerceBoundIntZFrac(source)
      case (source @ TBoundedInt(_, _), target: TFloat[G]) => CoerceBoundIntFloat(source, target)

      case (source @ TBoundedInt(gte, lt), target @ TBoundedInt(t_gte, t_lt)) if t_gte <= gte && t_lt >= lt =>
        CoerceWidenBound(source, target)
      case (source: TBoundedInt[G], target: TInt[G]) => CoerceUnboundInt(source, target)
      case (source: TBoundedInt[G], TRational()) => CoercionSequence(Seq(CoerceUnboundInt(source, TInt()), CoerceIntRat()))
      case (_: IntType[G], TRational()) => CoerceIntRat()

      case (source @ TClass(sourceClass), target @ TClass(targetClass))
        if source.transSupportArrows.exists { case (_, supp) => supp == targetClass.decl } =>
        CoerceSupports(sourceClass, targetClass)

      case (source @ TClass(sourceClass), TAnyClass()) =>
        CoerceClassAnyClass(sourceClass)

      case (source @ JavaTClass(sourceClass, Nil), target @ JavaTClass(targetClass, Nil))
        if sourceClass.decl.transSupportArrows(Set.empty).exists { case (_, supp) => supp == targetClass.decl } =>
        CoerceJavaSupports(sourceClass, targetClass)

      case (source @ JavaTClass(sourceClass, Nil), TAnyClass()) =>
        CoerceJavaClassAnyClass(sourceClass)

      case (source @ TUnion(ts), target) =>
        CoerceJoinUnion(ts.map(getAnyCoercion(_, target)).map {
          case Some(coercion) => coercion
          case None => return None
        }, source.types, target)
      case (source, target @ TUnion(ts)) =>
        return ts.map(getAnyCoercion(source, _)).zipWithIndex.collectFirst {
          case (Some(coercion), index) =>
            CoerceSelectUnion(coercion, source, target.types, index)
        }
      case (source @ TCFloat(_, _), TCInt()) => CoerceCFloatCInt(source)
      case (TCInt() , target @ TCFloat(_, _)) => CoerceCIntCFloat(target)

      case (source @ TCFloat(_, _), TInt()) => CoercionSequence(Seq(CoerceCFloatCInt(source), CoerceCIntInt()))
      case (TCInt() , target @ TFloat(exponent, mantissa)) =>
        val coercedCFloat = TCFloat[G](exponent, mantissa)
        CoercionSequence(Seq(CoerceCIntCFloat(coercedCFloat), CoerceCFloatFloat(coercedCFloat, target)))
      case (source @ CPrimitiveType(specs), target) =>
        specs.collectFirst { case spec: CSpecificationType[G] => spec } match {
          case Some(CSpecificationType(t)) =>
            CoercionSequence(Seq(
              CoerceCPrimitiveToCol(source, t),
              getAnyCoercion(t, target).getOrElse(return None),
            ))
          case None => return None
        }

      case (source, target @ CPrimitiveType(specs)) =>
        specs.collectFirst { case spec: CSpecificationType[G] => spec } match {
          case Some(CSpecificationType(t)) =>
            CoercionSequence(Seq(
              getAnyCoercion(source, t).getOrElse(return None),
              CoerceColToCPrimitive(t, target),
            ))
          case None => return None
        }

      case (source@CPPPrimitiveType(specs), target) =>
        specs.collectFirst { case spec: CPPSpecificationType[G] => spec } match {
          case Some(CPPSpecificationType(t)) =>
            CoercionSequence(Seq(
              CoerceCPPPrimitiveToCol(source, t),
              getAnyCoercion(t, target).getOrElse(return None),
            ))
          case None => return None
        }

      case (source, target@CPPPrimitiveType(specs)) =>
        specs.collectFirst { case spec: CPPSpecificationType[G] => spec } match {
          case Some(CPPSpecificationType(t)) =>
            CoercionSequence(Seq(
              getAnyCoercion(source, t).getOrElse(return None),
              CoerceColToCPPPrimitive(t, target),
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

  def getAnyCPPCoercion[G](source: Type[G]): Option[(Coercion[G], Type[G])] = source match {
    case t: CPPPrimitiveType[G] =>
      t.specifiers.collectFirst { case spec: CPPSpecificationType[G] => spec }.map {
        case CPPSpecificationType(inner) => (CoerceCPPPrimitiveToCol(t, inner), inner)
      }
    case _ => None
  }

  def chainCPPCoercion[G, T](source: CPPPrimitiveType[G], next: Type[G] => Option[(Coercion[G], T)]): Option[(Coercion[G], T)] =
    getAnyCPPCoercion(source) match {
      case Some(inner) => next(inner._2) match {
        case Some((coercion, finalType)) =>
          Some((CoercionSequence(Seq(coercion, inner._1)), finalType))
        case None => None
      }
      case None => None
    }

  def getAnySeqCoercion[G](source: Type[G]): Option[(Coercion[G], TSeq[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnySeqCoercion)
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnySeqCoercion)
    case t: TSeq[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnySetCoercion[G](source: Type[G]): Option[(Coercion[G], TSet[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnySetCoercion)
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnySetCoercion)
    case t: TSet[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnyBagCoercion[G](source: Type[G]): Option[(Coercion[G], TBag[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyBagCoercion)
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnyBagCoercion)
    case t: TBag[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnySizedCoercion[G](source: Type[G]): Option[(Coercion[G], SizedType[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnySizedCoercion)
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnySizedCoercion)
    case t: TSeq[G] => Some((CoerceIdentity(source), t))
    case t: TSet[G] => Some((CoerceIdentity(source), t))
    case t: TBag[G] => Some((CoerceIdentity(source), t))
    case t: TMap[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnyPointerCoercion[G](source: Type[G]): Option[(Coercion[G], TPointer[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyPointerCoercion)
    case t: TPointer[G] => Some((CoerceIdentity(source), t))
    case t: CTPointer[G] => Some((CoerceIdentity(source), TPointer(t.innerType)))
    case t: CTArray[G] => Some((CoerceCArrayPointer(t.innerType), TPointer(t.innerType)))
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnyPointerCoercion)
    case t: CPPTArray[G] => Some((CoerceCPPArrayPointer(t.innerType), TPointer(t.innerType)))
    case _: TNull[G] =>
      val t = TPointer[G](TAnyValue())
      Some((CoerceNullPointer(t), t))
    case _ => None
  }

  def getAnyCArrayCoercion[G](source: Type[G]): Option[(Coercion[G], CTArray[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyCArrayCoercion)
    case t: CTArray[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnyCPPArrayCoercion[G](source: Type[G]): Option[(Coercion[G], CPPTArray[G])] = source match {
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnyCPPArrayCoercion)
    case t: CPPTArray[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnyArrayCoercion[G](source: Type[G]): Option[(Coercion[G], TArray[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyArrayCoercion)
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnyArrayCoercion)
    case acc: SYCLTAccessor[G] => Some((
      CoerceIdentity(source),
      FuncTools.repeat(TArray[G](_), acc.dimCount, acc.typ).asInstanceOf[TArray[G]]
    ))
    case acc: SYCLTLocalAccessor[G] => Some((
      CoerceIdentity(source),
      FuncTools.repeat(TArray[G](_), acc.dimCount, acc.typ).asInstanceOf[TArray[G]]
    ))
    case t: TArray[G] => Some((CoerceIdentity(source), t))
    case _: TNull[G] =>
      val t = TArray[G](TAnyValue())
      Some((CoerceNullArray(t), t))
    case _ => None
  }

  def getAnyMatrixArrayCoercion[G](source: Type[G]): Option[(Coercion[G], TArray[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyMatrixArrayCoercion)
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnyMatrixArrayCoercion)
    case acc: SYCLTAccessor[G] if acc.dimCount >= 2 => Some((
      CoerceIdentity(source),
      FuncTools.repeat(TArray[G](_), acc.dimCount, acc.typ).asInstanceOf[TArray[G]]
    ))
    case acc: SYCLTLocalAccessor[G] if acc.dimCount >= 2 => Some((
      CoerceIdentity(source),
      FuncTools.repeat(TArray[G](_), acc.dimCount, acc.typ).asInstanceOf[TArray[G]]
    ))
    case t @ TArray(TArray(_)) => Some((CoerceIdentity(source), t))
    case TArray(TNull()) => Some(???)
    case TNull() =>
      val t = TArray[G](TArray[G](TAnyValue()))
      Some((CoerceNullArray(t), t))
    case _ => None
  }

  def getAnyOptionCoercion[G](source: Type[G]): Option[(Coercion[G], TOption[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyOptionCoercion)
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnyOptionCoercion)
    case t: TOption[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnyMapCoercion[G](source: Type[G]): Option[(Coercion[G], TMap[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyMapCoercion)
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnyMapCoercion)
    case t: TMap[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnyTupleCoercion[G](source: Type[G]): Option[(Coercion[G], TTuple[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyTupleCoercion)
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnyTupleCoercion)
    case t: TTuple[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnyMatrixCoercion[G](source: Type[G]): Option[(Coercion[G], TMatrix[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyMatrixCoercion)
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnyMatrixCoercion)
    case t: TMatrix[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnyModelCoercion[G](source: Type[G]): Option[(Coercion[G], TModel[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyModelCoercion)
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnyModelCoercion)
    case t: TModel[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnyClassCoercion[G](source: Type[G]): Option[(Coercion[G], TClass[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyClassCoercion)
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnyClassCoercion)
    case t: TClass[G] => Some((CoerceIdentity(source), t))

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

  def getAnyEitherCoercion[G](source: Type[G]): Option[(Coercion[G], TEither[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyEitherCoercion)
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnyEitherCoercion)
    case t: TEither[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnyBitvecCoercion[G](source: Type[G]): Option[(Coercion[G], TSmtlibBitVector[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnyBitvecCoercion)
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnyBitvecCoercion)
    case t: TSmtlibBitVector[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnySmtlibFloatCoercion[G](source: Type[G]): Option[(Coercion[G], TSmtlibFloatingPoint[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnySmtlibFloatCoercion)
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnySmtlibFloatCoercion)
    case t: TSmtlibFloatingPoint[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnySmtlibArrayCoercion[G](source: Type[G]): Option[(Coercion[G], TSmtlibArray[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnySmtlibArrayCoercion)
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnySmtlibArrayCoercion)
    case t: TSmtlibArray[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }

  def getAnySmtlibSeqCoercion[G](source: Type[G]): Option[(Coercion[G], TSmtlibSeq[G])] = source match {
    case t: CPrimitiveType[G] => chainCCoercion(t, getAnySmtlibSeqCoercion)
    case t: CPPPrimitiveType[G] => chainCPPCoercion(t, getAnySmtlibSeqCoercion)
    case t: TSmtlibSeq[G] => Some((CoerceIdentity(source), t))
    case _ => None
  }
}