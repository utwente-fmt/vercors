package vct.col.ast.expr.op.cmp

import vct.col.ast.{MapDisjoint, TBool, Type}
import vct.col.print.{Ctx, Doc, Precedence, Group}
import vct.col.ast.ops.MapDisjointOps

trait MapDisjointImpl[G] extends MapDisjointOps[G] { this: MapDisjoint[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(left) <> ".disjoint(" <> Doc.arg(right) <> ")")
}
