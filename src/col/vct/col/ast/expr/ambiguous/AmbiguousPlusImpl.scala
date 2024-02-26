package vct.col.ast.expr.ambiguous

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ref.Ref
import vct.col.typerules.{CoercionUtils, Types}
import vct.result.VerificationError.Unreachable
import vct.col.ast.ops.AmbiguousPlusOps

trait AmbiguousPlusImpl[G] extends AmbiguousPlusOps[G] { this: AmbiguousPlus[G] =>

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

  override lazy val t: Type[G] = {
    if(isProcessOp) TProcess()
    else if(isSeqOp || isBagOp || isSetOp || isVectorOp) Types.leastCommonSuperType(left.t, right.t)
    else if(isPointerOp) left.t
    else if(isStringOp) TString()
    else if(getCustomPlusOpType().isDefined) getCustomPlusOpType().get
    else getNumericType
  }

  override def precedence: Int = Precedence.ADDITIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "+", right)
}