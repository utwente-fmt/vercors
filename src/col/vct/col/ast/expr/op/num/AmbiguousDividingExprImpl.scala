package vct.col.ast.expr.op.num

import vct.col.ast.expr.op.BinOperatorTypes
import vct.col.ast.{AmbiguousDividingExpr, Type}

trait AmbiguousDividingExprImpl[G] { this: AmbiguousDividingExpr[G] =>
  def t: Type[G] = BinOperatorTypes.getNumericType(left.t, right.t, o)
}
