package vct.col.ast.temporaryimplpackage.expr.literal.constant

import vct.col.ast.{EmptyProcess, TProcess, Type}

trait EmptyProcessImpl { this: EmptyProcess =>
  override def t: Type = TProcess()
}