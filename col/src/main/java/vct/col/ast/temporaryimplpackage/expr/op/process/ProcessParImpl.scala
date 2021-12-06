package vct.col.ast.temporaryimplpackage.expr.op.process

import vct.col.ast.{ProcessPar, TProcess, Type}

trait ProcessParImpl { this: ProcessPar =>
  override def t: Type = TProcess()
}