package vct.col.ast.lang

import vct.col.ast.{JavaPlus, TInt, TProcess, TRational, TString, Type}
import vct.col.ast.`type`.TFloats
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

  def isJavaLangStringOp: Boolean = if (ctx.get.javaLangStringType().isDefined)
    left.t == javaStringClassType || right.t == javaStringClassType
  else false

  private lazy val javaStringClassType: Type[G] = ctx.get.javaLangStringType().get // This is a bit weird, with the get and the if a few lines up
  override lazy val t: Type[G] =
    if (isProcessOp) TProcess()
    else if (isSeqOp || isBagOp || isSetOp) Types.leastCommonSuperType(left.t, right.t)
    else if (isPointerOp) left.t
    else if (TFloats.isFloatOp(left.t, right.t))
      TFloats.coerceToMax[G](left.t, right.t)
    else if (isIntOp) TInt()
    else if (isStringOp) TString()
    else if (isJavaLangStringOp) javaStringClassType
    else TRational()
}
