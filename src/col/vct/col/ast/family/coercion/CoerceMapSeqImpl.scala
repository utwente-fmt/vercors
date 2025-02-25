package vct.col.ast.family.coercion

import vct.col.ast.{CoerceMapSeq, TSeq}
import vct.col.ast.ops.CoerceMapSeqOps

trait CoerceMapSeqImpl[G] extends CoerceMapSeqOps[G] {
  this: CoerceMapSeq[G] =>
  override def target: TSeq[G] = TSeq(targetSeqElement)
}
