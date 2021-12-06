package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{Concat, TSeq, Type}
import vct.col.util.Types

trait ConcatImpl { this: Concat =>
  def leftType: TSeq = xs.t.asSeq.get

  def rightType: TSeq = ys.t.asSeq.get

  override def t: Type = TSeq(Types.leastCommonSuperType(leftType.element, rightType.element))
}