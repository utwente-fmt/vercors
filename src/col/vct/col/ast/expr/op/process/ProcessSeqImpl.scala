package vct.col.ast.expr.op.process

import vct.col.ast.{ProcessSeq, TProcess, Type}

trait ProcessSeqImpl[G] { this: ProcessSeq[G] =>
  override def t: Type[G] = TProcess()
}