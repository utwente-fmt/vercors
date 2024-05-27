package vct.col.ast.lang.smt

import vct.col.ast.SmtLib
import vct.col.ast.ops.SmtLibOps
import vct.col.print._

trait SmtLibImpl[G] extends SmtLibOps[G] {
  this: SmtLib[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
