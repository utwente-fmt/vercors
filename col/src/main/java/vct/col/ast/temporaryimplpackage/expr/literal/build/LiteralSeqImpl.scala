package vct.col.ast.temporaryimplpackage.expr.literal.build

import vct.col.ast.{LiteralSeq, TSeq, Type}

trait LiteralSeqImpl { this: LiteralSeq =>
  override def t: Type = TSeq(element)
}