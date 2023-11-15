package vct.col.ast.expr.op

import vct.col.ast.`type`.typeclass.TFloats.getFloatMax
import vct.col.ast.{BinExpr, Expr, TBool, TCInt, TInt, TProcess, TRational, TString, Type}
import vct.col.typerules.{CoercionUtils, Types}
import vct.result.VerificationError

trait BinExprImpl[G] { this: BinExpr[G] =>
  def left: Expr[G]
  def right: Expr[G]

  case class NumericBinError() extends VerificationError.UserError {
    override def text: String = o.messageInContext(f"Expected types to numeric, but got: ${left.t} and ${right.t}")
    override def code: String = "numericBinError"
  }

  def isCIntOp: Boolean =
    CoercionUtils.getCoercion(left.t, TCInt()).isDefined &&
      CoercionUtils.getCoercion(right.t, TCInt()).isDefined

  def isIntOp: Boolean =
    CoercionUtils.getCoercion(left.t, TInt()).isDefined &&
      CoercionUtils.getCoercion(right.t, TInt()).isDefined

  def isRationalOp: Boolean =
    CoercionUtils.getCoercion(left.t, TRational()).isDefined &&
      CoercionUtils.getCoercion(right.t, TRational()).isDefined

  def isBoolOp: Boolean = CoercionUtils.getCoercion(left.t, TBool()).isDefined &&
    CoercionUtils.getCoercion(right.t, TBool()).isDefined

  def isStringOp: Boolean = CoercionUtils.getCoercion(left.t, TString()).isDefined

  def isBagOp: Boolean = CoercionUtils.getAnyBagCoercion(left.t).isDefined
  def isPointerOp: Boolean = CoercionUtils.getAnyPointerCoercion(left.t).isDefined
  def isProcessOp: Boolean = CoercionUtils.getCoercion(left.t, TProcess()).isDefined
  def isSeqOp: Boolean = CoercionUtils.getAnySeqCoercion(left.t).isDefined
  def isSetOp: Boolean = CoercionUtils.getAnySetCoercion(left.t).isDefined


  def getNumericType: Type[G] = {
    if (isCIntOp) TCInt[G]() else
    if(isIntOp) TInt[G]() else
      getFloatMax[G](left.t, right.t) getOrElse (
        if(isRationalOp) TRational[G]()
        else throw NumericBinError()
      )
  }
}