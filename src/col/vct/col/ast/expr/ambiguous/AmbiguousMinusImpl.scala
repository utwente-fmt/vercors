package vct.col.ast.expr.ambiguous

import vct.col.ast.{AmbiguousMinus, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.typerules.{CoercionUtils, Types}
import vct.col.ast.ops.AmbiguousMinusOps

trait AmbiguousMinusImpl[G] extends AmbiguousMinusOps[G] {
  this: AmbiguousMinus[G] =>

  override lazy val t: Type[G] = {
    if (isSetOp || isBagOp || isVectorOp)
      Types.leastCommonSuperType(left.t, right.t)
    else if (isPointerOp)
      left.t
    else
      getNumericType
  }

  override def precedence: Int = Precedence.ADDITIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "-", right)
}
