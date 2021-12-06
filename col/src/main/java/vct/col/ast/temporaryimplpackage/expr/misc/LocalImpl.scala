package vct.col.ast.temporaryimplpackage.expr.misc

import vct.col.ast.{Local, Type}
import vct.col.check.{CheckContext, CheckError}

trait LocalImpl { this: Local =>
  override def t: Type = ref.decl.t
  override def check(context: CheckContext): Seq[CheckError] =
    context.checkInScope(this, ref)
}