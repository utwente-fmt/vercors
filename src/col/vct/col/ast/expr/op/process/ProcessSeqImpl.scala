package vct.col.ast.expr.op.process

import vct.col.ast.{ProcessSeq, TProcess, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait ProcessSeqImpl[G] {
  this: ProcessSeq[G] =>
  override def t: Type[G] = TProcess()

  override def precedence: Int = Precedence.MULTIPLICATIVE
  override def layout(implicit ctx: Ctx): Doc = lassoc(left, "*", right)
}
