package vct.col.ast.expr.lock

import vct.col.ast.{Committed, TBool}

trait CommittedImpl[G] { this: Committed[G] =>
  override def t: TBool[G] = TBool()
}
