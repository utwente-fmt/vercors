package vct.col.ast.family.coercion

import vct.col.ast.{CoerceMapSeq, TSeq}

trait CoerceMapSeqImpl[G] { this: CoerceMapSeq[G] => 
  override def target: TSeq[G] = TSeq(targetSeqElement)
}
