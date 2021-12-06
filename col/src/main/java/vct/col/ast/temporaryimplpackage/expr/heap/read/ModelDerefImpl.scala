package vct.col.ast.temporaryimplpackage.expr.heap.read

import vct.col.ast.{ModelDeref, Type}
import vct.col.check.{CheckContext, CheckError}

trait ModelDerefImpl { this: ModelDeref =>
  override def t: Type = ref.decl.t
  override def check(context: CheckContext): Seq[CheckError] =
    context.checkInScope(this, ref)
}