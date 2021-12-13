package vct.col.ast.temporaryimplpackage.expr.literal.build

import vct.col.ast.{LiteralSeq, TSeq, Type}

trait LiteralSeqImpl[G] { this: LiteralSeq[G] =>
  override def t: Type[G] = TSeq(element)
}