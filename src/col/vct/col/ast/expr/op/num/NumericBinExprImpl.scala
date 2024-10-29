package vct.col.ast.expr.op.num

import vct.col.ast.{NumericBinExpr, TInt, TRational, Type}
import vct.col.typerules.CoercionUtils

trait NumericBinExprImpl[G] {
  this: NumericBinExpr[G] =>
  lazy val t: Type[G] = getNumericType
}
