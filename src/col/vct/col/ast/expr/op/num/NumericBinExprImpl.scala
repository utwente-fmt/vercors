package vct.col.ast.expr.op.num

import vct.col.ast.{NumericBinExpr, TInt, TRational, Type}
import vct.col.typerules.CoercionUtils

trait NumericBinExprImpl[G] { this: NumericBinExpr[G] =>

  override def t: Type[G] = {
    getNumericType
  }
}