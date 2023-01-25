package vct.col.ast.expr.ambiguous

import vct.col.ast._
import vct.col.ast.`type`.TFloats
import vct.col.typerules.{CoercionUtils, Types}

trait AmbiguousPlusImpl[G] { this: AmbiguousPlus[G] =>
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
  def isJavaLangStringOp: Boolean =
    ???
//    CoercionUtils.getCoercion(left.t, TPinnedDecl(JavaLangString(), Nil)).isDefined


  override lazy val t: Type[G] =
    if(isProcessOp) TProcess()
    else if(isSeqOp || isBagOp || isSetOp) Types.leastCommonSuperType(left.t, right.t)
    else if(isPointerOp) left.t
    else if(TFloats.isFloatOp(left.t, right.t))
      TFloats.coerceToMax[G](left.t, right.t)
    else if(isIntOp) TInt()
    else if(isStringOp) TString()
    else if(isJavaLangStringOp) ???
    else TRational()
}