package vct.col.ast.expr.literal.build

import vct.col.ast.{LiteralSeq, TSeq, Type}

trait LiteralSeqImpl[G] { this: LiteralSeq[G] =>
  override def t: Type[G] = TSeq(element)
}