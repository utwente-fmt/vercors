package vct.col.ast.expr.literal.constant

import vct.col.ast.{EmptyProcess, TProcess, Type}

trait EmptyProcessImpl[G] { this: EmptyProcess[G] =>
  override def t: Type[G] = TProcess()
}