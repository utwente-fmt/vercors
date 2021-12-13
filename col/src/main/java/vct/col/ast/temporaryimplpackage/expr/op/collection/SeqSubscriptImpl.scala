package vct.col.ast.temporaryimplpackage.expr.op.collection

import vct.col.ast.{SeqSubscript, Type}

trait SeqSubscriptImpl[G] { this: SeqSubscript[G] =>
  override def t: Type[G] = seq.t.asSeq.get.element
}