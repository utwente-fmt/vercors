package vct.col.ast.lang

import vct.col.ast.{JavaClass, JavaPlus, JavaTClass, TInt, TProcess, TRational, TString, Type}
import vct.col.ast.`type`.TFloats
import vct.col.ref.Ref
import vct.col.typerules.{CoercionUtils, Types}

trait JavaPlusImpl[G] { this: JavaPlus[G] =>

  // Oopsie oopsie oopsie... Surely we can factor out the common parts...
  def isProcessOp: Boolean = CoercionUtils.getCoercion(left.t, TProcess()).isDefined

  def isSeqOp: Boolean = CoercionUtils.getAnySeqCoercion(left.t).isDefined

  def isBagOp: Boolean = CoercionUtils.getAnyBagCoercion(left.t).isDefined

  def isSetOp: Boolean = CoercionUtils.getAnySetCoercion(left.t).isDefined

  def isPointerOp: Boolean = CoercionUtils.getAnyPointerCoercion(left.t).isDefined

  def isIntOp: Boolean =
    CoercionUtils.getCoercion(left.t, TInt()).isDefined &&
      CoercionUtils.getCoercion(right.t, TInt()).isDefined

  def isStringOp: Boolean =
    CoercionUtils.getCoercion(left.t, TString()).isDefined

  def getJavaStringClass: Option[JavaTClass[G]] =
    (left.t match {
      case t @ JavaTClass(Ref(cls: JavaClass[G]), Seq()) if cls.isJavaStringClass => Some(t)
      case _ => None
    }).orElse(right.t match {
        case t @ JavaTClass(Ref(cls: JavaClass[G]), Seq()) if cls.isJavaStringClass => Some(t)
        case _ => None
      })

  def isJavaStringClassOp: Boolean = getJavaStringClass.isDefined

  override lazy val t: Type[G] =
    if (isProcessOp) TProcess()
    else if (isSeqOp || isBagOp || isSetOp) Types.leastCommonSuperType(left.t, right.t)
    else if (isPointerOp) left.t
    else if (TFloats.isFloatOp(left.t, right.t))
      TFloats.coerceToMax[G](left.t, right.t)
    else if (isIntOp) TInt()
    else if (isStringOp) TString()
    else if (isJavaStringClassOp) getJavaStringClass.get
    else TRational()
}
