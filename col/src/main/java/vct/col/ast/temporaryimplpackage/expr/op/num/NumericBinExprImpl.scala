package vct.col.ast.temporaryimplpackage.expr.op.num

import vct.col.ast.{NumericBinExpr, TInt, TRational, Type}
import vct.col.coerce.Coercion

trait NumericBinExprImpl[G] { this: NumericBinExpr[G] =>
  def isIntOp: Boolean =
    Coercion.getCoercion(left.t, TInt()).isDefined &&
      Coercion.getCoercion(right.t, TInt()).isDefined

  override def t: Type[G] = if(isIntOp) TInt() else TRational()
}