package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReRepeatRange, TSmtlibRegLan, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibReRepeatRangeOps

trait SmtlibReRepeatRangeImpl[G] extends SmtlibReRepeatRangeOps[G] { this: SmtlibReRepeatRange[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
