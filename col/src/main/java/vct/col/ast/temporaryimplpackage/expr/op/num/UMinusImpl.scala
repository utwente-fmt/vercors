package vct.col.ast.temporaryimplpackage.expr.op.num

import vct.col.ast.{TInt, TRational, Type, UMinus}
import vct.col.coerce.CoercionUtils

trait UMinusImpl[G] { this: UMinus[G] =>
  override def t: Type[G] =
    CoercionUtils.getCoercion(arg.t, TInt()) match {
      case Some(_) => TInt()
      case _ => TRational()
    }
}