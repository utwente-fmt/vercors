package vct.col.ast.temporaryimplpackage.expr.op.process

import vct.col.ast.{ProcessSelect, TProcess, Type}

trait ProcessSelectImpl[G] { this: ProcessSelect[G] =>
  override def t: Type[G] = TProcess()
}