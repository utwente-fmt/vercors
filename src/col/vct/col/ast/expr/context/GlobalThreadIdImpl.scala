package vct.col.ast.expr.context

import vct.col.ast.{GlobalThreadId, TInt}

trait GlobalThreadIdImpl[G] { this: GlobalThreadId[G] =>
  override def t: TInt[G] = TInt()
}
