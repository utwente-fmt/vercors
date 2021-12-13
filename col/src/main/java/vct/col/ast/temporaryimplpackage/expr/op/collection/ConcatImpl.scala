package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{Concat, TSeq, Type}
import vct.col.util.Types

trait ConcatImpl[G] { this: Concat[G] =>
  def leftType: TSeq[G] = xs.t.asSeq.get

  def rightType: TSeq[G] = ys.t.asSeq.get

  override def t: Type[G] = TSeq(Types.leastCommonSuperType(leftType.element, rightType.element))
}