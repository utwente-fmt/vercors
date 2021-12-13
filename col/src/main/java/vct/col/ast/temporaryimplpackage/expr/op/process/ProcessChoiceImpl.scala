package vct.col.ast.temporaryimplpackage.expr.op.process

import vct.col.ast.{ProcessChoice, TProcess, Type}

trait ProcessChoiceImpl[G] { this: ProcessChoice[G] =>
  override def t: Type[G] = TProcess()
}