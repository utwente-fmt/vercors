package vct.col.ast.temporaryimplpackage.expr.context

import vct.col.ast.{CurrentThreadId, TInt, Type}

trait CurrentThreadIdImpl[G] { this: CurrentThreadId[G] =>
  override def t: Type[G] = TInt()
}