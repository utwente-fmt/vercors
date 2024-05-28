package vct.col.ast.expr.op.num

import vct.col.ast.expr.op.BinOperatorTypes
import vct.col.ast.{AmbiguousDividingExpr, Type}
import vct.col.typerules.Types

trait AmbiguousDividingExprImpl[G] {
  this: AmbiguousDividingExpr[G] =>
  def t: Type[G] =
    if (isVectorOp)
      Types.leastCommonSuperType(left.t, right.t)
    else
      BinOperatorTypes.getNumericType(left.t, right.t, o)
}
