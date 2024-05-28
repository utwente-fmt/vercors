package vct.col.ast.expr.op.num

import vct.col.ast.`type`.typeclass.TFloats
import vct.col.ast.{FloatDiv, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.FloatDivOps

trait FloatDivImpl[G] extends FloatDivOps[G] {
  this: FloatDiv[G] =>
  override def t: Type[G] = TFloats.getFloatMax(left.t, right.t).get

  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "/", right)
}
