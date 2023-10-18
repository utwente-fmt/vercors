package vct.col.ast.expr.op.collection

import vct.col.ast.{SeqSubscript, Type}
import vct.col.print.{Ctx, Doc, Precedence, Group}

trait SeqSubscriptImpl[G] {
  this: SeqSubscript[G] =>
  override def t: Type[G] = seq.t.asSeq.get.element

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(seq) <> "[" <> Doc.arg(index) <> "]")
}
