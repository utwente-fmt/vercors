package vct.col.rewrite

import vct.col.ast.Declaration

class SuccessorProviderChain[A, B, C, AD <: Declaration[A], BD <: Declaration[
  B
], CD <: Declaration[C]](
    left: SuccessorProvider[A, B, AD, BD],
    right: SuccessorProvider[B, C, BD, CD],
) extends SuccessorProvider[A, C, AD, CD] {
  override def computeSucc(decl: AD): Option[CD] =
    left.computeSucc(decl).flatMap(right.computeSucc)
}
