package vct.col.ast.temporaryimplpackage.expr.misc

import vct.col.ast.{CurrentThreadId, TInt, Type}

trait CurrentThreadIdImpl { this: CurrentThreadId =>
  override def t: Type = TInt()
}