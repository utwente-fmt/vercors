package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{Cons, TSeq}
import vct.col.util.Types

trait ConsImpl { this: Cons =>
  def tailType: TSeq = xs.t.asSeq.get

  override def t: TSeq = TSeq(Types.leastCommonSuperType(tailType.element, x.t))
}