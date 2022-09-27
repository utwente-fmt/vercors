package vct.col.ast.statement.composite

import vct.col.ast.ParAtomic
import vct.col.check.{CheckContext, CheckError}

trait ParAtomicImpl[G] { this: ParAtomic[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] =
    inv.flatMap(context.checkInScope(this, _))
}