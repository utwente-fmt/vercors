package vct.col.ast.temporaryimplpackage.expr.op.process

import vct.col.ast.{ProcessSeq, TProcess, Type}

trait ProcessSeqImpl { this: ProcessSeq =>
  override def t: Type = TProcess()
}