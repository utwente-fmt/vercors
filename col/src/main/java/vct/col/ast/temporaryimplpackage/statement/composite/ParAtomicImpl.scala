package vct.col.ast.temporaryimplpackage.statement.composite

import vct.col.ast.ParAtomic
import vct.col.check.{CheckContext, CheckError}

trait ParAtomicImpl { this: ParAtomic =>
  override def check(context: CheckContext): Seq[CheckError] =
    inv.flatMap(context.checkInScope(this, _))
}