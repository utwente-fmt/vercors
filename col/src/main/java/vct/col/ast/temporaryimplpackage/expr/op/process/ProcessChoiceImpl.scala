package vct.col.ast.temporaryimplpackage.expr.op.process

import vct.col.ast.{ProcessChoice, TProcess, Type}

trait ProcessChoiceImpl { this: ProcessChoice =>
  override def t: Type = TProcess()
}