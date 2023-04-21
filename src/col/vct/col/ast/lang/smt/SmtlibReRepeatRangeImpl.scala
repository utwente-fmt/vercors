package vct.col.ast.lang.smt

import vct.col.ast.SmtlibReRepeatRange
import vct.col.ast.Type
import vct.col.print._

trait SmtlibReRepeatRangeImpl[G] { this: SmtlibReRepeatRange[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
