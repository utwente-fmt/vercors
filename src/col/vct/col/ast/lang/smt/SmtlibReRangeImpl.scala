package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReRange, TSmtlibRegLan, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibReRangeOps

trait SmtlibReRangeImpl[G] extends SmtlibReRangeOps[G] { this: SmtlibReRange[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
