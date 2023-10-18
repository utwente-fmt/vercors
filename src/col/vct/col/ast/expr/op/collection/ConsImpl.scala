package vct.col.ast.expr.op.collection

import vct.col.ast.{Cons, TSeq}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.typerules.Types

trait ConsImpl[G] {
  this: Cons[G] =>
  def tailType: TSeq[G] = xs.t.asSeq.get

  override lazy val t: TSeq[G] = TSeq(
    Types.leastCommonSuperType(tailType.element, x.t)
  )

  override def precedence: Int = Precedence.SEQUENCE
  override def layout(implicit ctx: Ctx): Doc = rassoc(x, "::", xs)
}
