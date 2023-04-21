package vct.col.ast.lang.smt

import vct.col.ast.SmtlibReDiff
import vct.col.ast.Type
import vct.col.print._

trait SmtlibReDiffImpl[G] { this: SmtlibReDiff[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
