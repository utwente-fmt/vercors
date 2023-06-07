package vct.col.ast.lang.smt

import vct.col.ast.SmtlibSelect
import vct.col.ast.Type
import vct.col.print._

trait SmtlibSelectImpl[G] { this: SmtlibSelect[G] =>
  override def t: Type[G] = arr.t.asSmtlibArray.get.value
  // def layout(implicit ctx: Ctx): Doc = ???
}
