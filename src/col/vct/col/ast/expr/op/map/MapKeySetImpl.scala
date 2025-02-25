package vct.col.ast.expr.op.map

import vct.col.ast.{MapKeySet, TSet, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.MapKeySetOps

trait MapKeySetImpl[G] extends MapKeySetOps[G] {
  this: MapKeySet[G] =>
  override def t: Type[G] = TSet(mapType.key)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = assoc(map) <> ".keys"
}
