package vct.col.ast.temporaryimplpackage.expr.op.num

import vct.col.ast.{TInt, TRational, Type, UMinus}
import vct.col.coerce.Coercion

trait UMinusImpl { this: UMinus =>
  override def t: Type =
    Coercion.getCoercion(arg.t, TInt()) match {
      case Some(_) => TInt()
      case _ => TRational()
    }
}