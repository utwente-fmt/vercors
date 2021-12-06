package vct.col.ast.temporaryimplpackage.expr.heap.read

import vct.col.ast.{Deref, Type}
import vct.col.check.{CheckContext, CheckError}

trait DerefImpl { this: Deref =>
  override def t: Type = ref.decl.t
  override def check(context: CheckContext): Seq[CheckError] =
    context.checkInScope(this, ref)
}