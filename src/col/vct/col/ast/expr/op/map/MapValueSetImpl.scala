package vct.col.ast.expr.op.map

import vct.col.ast.{MapValueSet, TSet, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.MapValueSetOps

trait MapValueSetImpl[G] extends MapValueSetOps[G] {
  this: MapValueSet[G] =>
  override def t: Type[G] = TSet(mapType.value)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = assoc(map) <> ".values"
}
