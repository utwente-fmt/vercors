package vct.col.ast.lang.smt

import vct.col.ast.SmtlibReRange
import vct.col.ast.Type
import vct.col.print._

trait SmtlibReRangeImpl[G] { this: SmtlibReRange[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
