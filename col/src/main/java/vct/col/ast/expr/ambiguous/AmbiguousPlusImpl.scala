package vct.col.ast.expr.ambiguous

import vct.col.ast._
import vct.col.ast.`type`.TFloats
import vct.col.ref.Ref
import vct.col.typerules.{CoercionUtils, Types}
import vct.result.VerificationError.Unreachable

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

  def getValidOperatorsOf(operator: Operator[G]): Option[Seq[ContractApplicable[G]]] = {
    val subject = if(operator == OperatorLeftPlus[G]()) left else right
    val decls = subject.t match {
      case TClass(Ref(cls)) => cls.declarations
      case JavaTClass(Ref(cls), _) => cls.declarations
      case _ => return None
    }
    Some(decls.collect {
      case m: InstanceOperatorMethod[G] if m.operator == operator
        && CoercionUtils.getCoercion(right.t, m.args.head.t).isDefined => m
      case f: InstanceOperatorFunction[G] if f.operator == operator
        && CoercionUtils.getCoercion(right.t, f.args.head.t).isDefined => f
    })
  }

  def getCustomPlusType(operator: Operator[G]): Option[Type[G]] = {
    getValidOperatorsOf(operator) match {
      case Some(Seq(op)) => Some(op.returnType)
      case Some(Seq()) => None
      case None => None
      case _ => throw Unreachable("Multiple options possible")
    }
  }

  def getCustomPlusOpType(): Option[Type[G]] =
    getCustomPlusType(OperatorLeftPlus[G]())
      .orElse(getCustomPlusType(OperatorRightPlus[G]()))

  override lazy val t: Type[G] =
    if(isProcessOp) TProcess()
    else if(isSeqOp || isBagOp || isSetOp) Types.leastCommonSuperType(left.t, right.t)
    else if(isPointerOp) left.t
    else if(TFloats.isFloatOp(left.t, right.t))
      TFloats.coerceToMax[G](left.t, right.t)
    else if(isIntOp) TInt()
    else if(isStringOp) TString()
    else if(getCustomPlusOpType().isDefined) getCustomPlusOpType().get
    else TRational()
}