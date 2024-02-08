package vct.col.ast.expr.op

import vct.col.ast.`type`.typeclass.TFloats.getFloatMax
import vct.col.ast.{BinExpr, Expr, IntType, TBool, TCInt, TInt, TProcess, TRational, TString, TVector, Type}
import vct.col.origin.Origin
import vct.col.typerules.{CoercionUtils, Types}
import vct.result.VerificationError

object BinOperatorTypes {
  def isCIntOp[G](lt: Type[G], rt: Type[G]): Boolean =
    CoercionUtils.getCoercion(lt, TCInt()).isDefined &&
      CoercionUtils.getCoercion(rt, TCInt()).isDefined

  def isIntOp[G](lt: Type[G], rt: Type[G]): Boolean =
    CoercionUtils.getCoercion(lt, TInt()).isDefined &&
      CoercionUtils.getCoercion(rt, TInt()).isDefined

  def isRationalOp[G](lt: Type[G], rt: Type[G]): Boolean =
    CoercionUtils.getCoercion(lt, TRational()).isDefined &&
      CoercionUtils.getCoercion(rt, TRational()).isDefined

  def isBoolOp[G](lt: Type[G], rt: Type[G]): Boolean = CoercionUtils.getCoercion(lt, TBool()).isDefined &&
    CoercionUtils.getCoercion(rt, TBool()).isDefined

  def isStringOp[G](lt: Type[G], rt: Type[G]): Boolean = CoercionUtils.getCoercion(lt, TString()).isDefined

  def isBagOp[G](lt: Type[G], rt: Type[G]): Boolean = CoercionUtils.getAnyBagCoercion(lt).isDefined
  def isPointerOp[G](lt: Type[G], rt: Type[G]): Boolean = CoercionUtils.getAnyPointerCoercion(lt).isDefined
  def isProcessOp[G](lt: Type[G], rt: Type[G]): Boolean = CoercionUtils.getCoercion(lt, TProcess()).isDefined
  def isSeqOp[G](lt: Type[G], rt: Type[G]): Boolean = CoercionUtils.getAnySeqCoercion(lt).isDefined
  def isSetOp[G](lt: Type[G], rt: Type[G]): Boolean = CoercionUtils.getAnySetCoercion(lt).isDefined
  def isVectorOp[G](lt: Type[G], rt: Type[G]): Boolean = CoercionUtils.getAnyVectorCoercion(lt).isDefined

  def isVectorIntOp[G](lt: Type[G], rt: Type[G]): Boolean = {
    (for{
      (_, TVector(sizeL, eL)) <- CoercionUtils.getAnyVectorCoercion(lt)
      (_, TVector(sizeR, eR)) <- CoercionUtils.getAnyVectorCoercion(rt)
    } yield if(sizeL!=sizeR) ??? else isIntOp(eL, eR))
    .getOrElse(false)
  }

  def getVectorType[G](lt: Type[G], rt: Type[G], o: Origin): Type[G] = {
    (for{
      (_, TVector(sizeL, eL)) <- CoercionUtils.getAnyVectorCoercion(lt)
      (_, TVector(sizeR, eR)) <- CoercionUtils.getAnyVectorCoercion(rt)
    } yield if(sizeL!=sizeR) ??? else TVector[G](sizeL, getNumericType(eL, eR, o)))
      .get
  }

  def getIntType[G](lt: Type[G], rt: Type[G]): IntType[G] = if(isCIntOp(lt, rt)) TCInt() else TInt()

  case class NumericBinError(lt: Type[_], rt: Type[_], o: Origin) extends VerificationError.UserError {
    override def text: String = o.messageInContext(f"Expected types to numeric, but got: ${lt} and ${rt}")
    override def code: String = "numericBinError"
  }

  def getNumericType[G](lt: Type[G], rt: Type[G], o: Origin): Type[G] = {
    if (isCIntOp(lt, rt)) TCInt[G]() else
      if(isIntOp(lt, rt)) TInt[G]() else
        getFloatMax[G](lt, rt) getOrElse (
          if(isRationalOp(lt, rt)) TRational[G]()
          else throw NumericBinError(lt, rt, o)
          )
  }
}

trait BinExprImpl[G] { this: BinExpr[G] =>
  def left: Expr[G]
  def right: Expr[G]

  def isCIntOp: Boolean = BinOperatorTypes.isCIntOp(left.t, right.t)

  def isIntOp: Boolean = BinOperatorTypes.isIntOp(left.t, right.t)

  def isRationalOp: Boolean = BinOperatorTypes.isRationalOp(left.t, right.t)

  def isBoolOp: Boolean = BinOperatorTypes.isBoolOp(left.t, right.t)

  def isStringOp: Boolean = BinOperatorTypes.isStringOp(left.t, right.t)

  def isBagOp: Boolean = BinOperatorTypes.isBagOp(left.t, right.t)
  def isPointerOp: Boolean = BinOperatorTypes.isPointerOp(left.t, right.t)
  def isProcessOp: Boolean = BinOperatorTypes.isProcessOp(left.t, right.t)
  def isSeqOp: Boolean = BinOperatorTypes.isSeqOp(left.t, right.t)
  def isSetOp: Boolean = BinOperatorTypes.isSetOp(left.t, right.t)
  def isVectorOp: Boolean = BinOperatorTypes.isVectorOp(left.t, right.t)
  def isVectorIntOp: Boolean = BinOperatorTypes.isVectorIntOp(left.t, right.t)

  def getIntType: IntType[G] = BinOperatorTypes.getIntType(left.t, right.t)

  def getNumericType: Type[G] = BinOperatorTypes.getNumericType(left.t, right.t, o)

  def getVectorType: Type[G] = BinOperatorTypes.getVectorType(left.t, right.t, o)
}