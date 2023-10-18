package vct.col.ast.expr.op.collection

import vct.col.ast.{Concat, TSeq, Type}
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.typerules.Types

trait ConcatImpl[G] {
  this: Concat[G] =>
  def leftType: TSeq[G] = xs.t.asSeq.get
  def rightType: TSeq[G] = ys.t.asSeq.get

  override lazy val t: Type[G] = TSeq(
    Types.leastCommonSuperType(leftType.element, rightType.element)
  )

  override def precedence: Int = Precedence.ADDITIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(xs, "+", ys)
}
