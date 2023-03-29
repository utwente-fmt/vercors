package vct.col.ast.expr.op.process

import vct.col.ast.{ProcessPar, TProcess, Type}

trait ProcessParImpl[G] { this: ProcessPar[G] =>
  override def t: Type[G] = TProcess()
}