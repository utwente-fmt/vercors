package vct.col.ast.temporaryimplpackage.expr.op.process

import vct.col.ast.{ProcessSelect, TProcess, Type}

trait ProcessSelectImpl { this: ProcessSelect =>
  override def t: Type = TProcess()
}