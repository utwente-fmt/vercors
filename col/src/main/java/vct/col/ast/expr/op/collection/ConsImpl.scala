package vct.col.ast.expr.op.collection

import vct.col.ast.{Cons, TSeq}
import vct.col.typerules.Types

trait ConsImpl[G] { this: Cons[G] =>
  def tailType: TSeq[G] = xs.t.asSeq.get

  override def t: TSeq[G] = TSeq(Types.leastCommonSuperType(tailType.element, x.t))
}