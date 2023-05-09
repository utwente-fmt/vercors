package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvShl
import vct.col.ast.Type
import vct.col.print._

trait SmtlibBvShlImpl[G] { this: SmtlibBvShl[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
