package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{SeqSubscript, Type}

trait SeqSubscriptImpl { this: SeqSubscript =>
  override def t: Type = seq.t.asSeq.get.element
}