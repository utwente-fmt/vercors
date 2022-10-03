package vct.col.ast.expr.context

import vct.col.ast.{LocalThreadId, TInt}

trait LocalThreadIdImpl[G] { this: LocalThreadId[G] =>
  override def t: TInt[G] = TInt()
}
