package vct.col.ast.expr.op.num

import vct.col.ast.{
  CoerceCIntInt,
  CoerceFloatRat,
  CoerceUnboundInt,
  TBoundedInt,
  TCInt,
  TInt,
  TRational,
  Type,
  UMinus,
}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.typerules.CoercionUtils
import vct.col.ast.ops.UMinusOps

trait UMinusImpl[G] extends UMinusOps[G] {
  this: UMinus[G] =>
  override def t: Type[G] = {
    CoercionUtils.getCoercion(arg.t, TInt()) match {
      case Some(CoerceUnboundInt(TBoundedInt(gte, lt), _)) =>
        TBoundedInt(-lt + 1, -gte + 1)
      case _ => arg.t
    }
  }

  override def precedence: Int = Precedence.PREFIX
  override def layout(implicit ctx: Ctx): Doc = Text("-") <> assoc(arg)
}
